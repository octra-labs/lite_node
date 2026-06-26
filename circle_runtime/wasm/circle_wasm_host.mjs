/*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*/


import {createHash} from 'node:crypto'
import {execFileSync} from 'node:child_process'
import {existsSync} from 'node:fs'
import path from 'node:path'
import {fileURLToPath} from 'node:url'

const utf8 = new TextEncoder()
const utf8Decoder = new TextDecoder()
const requestMagic = 'OCWR1'
const responseMagic = 'OCWS1'
const maxResponseBytes = 2 * 1024 * 1024
const maxSpawnPayloadJsonBytes = 2 * 1024 * 1024
const maxAssetEffectBodyB64Bytes = 1398104
const pageBytes = 65536
const maxInitialPages = 512
const hfheHelperEnvVar = 'OCTRA_CIRCLE_HFHE_HOST'
const wasmModuleDir = path.dirname(fileURLToPath(import.meta.url))

const allowedImports = new Set([
  'host_response_reset',
  'host_response_write',
  'host_response_finish',
  'host_circle_invoke_len',
  'host_circle_invoke',
  'host_hfhe_invoke_len',
  'host_hfhe_invoke',
  'host_kv_get_len',
  'host_kv_get',
  'host_kv_put',
  'host_kv_del',
  'host_epoch',
  'host_caller_len',
  'host_caller_read',
  'host_self_len',
  'host_self_read',
  'host_state_path_key_len',
  'host_state_path_key',
  'host_emit_event'
])

const readStdin = async () => {
  const chunks = []
  for await (const chunk of process.stdin) {
    chunks.push(Buffer.from(chunk))
  }
  return Buffer.concat(chunks).toString('utf8')
}

const writeJson = (value) => {
  process.stdout.write(`${JSON.stringify(value)}\n`)
}

const fail = (message, code = 1) => {
  process.stderr.write(`${message}\n`)
  process.exit(code)
}

const helperCandidates = (dir) => ([
  path.join(dir, '_build', 'default', 'bin', 'circle_hfhe_host.exe'),
  path.join(dir, '_build', 'default', 'bin', 'circle_hfhe_host'),
  path.join(dir, 'bin', 'circle_hfhe_host.exe'),
  path.join(dir, 'bin', 'circle_hfhe_host'),
])

const resolveHfheHelperPath = () => {
  const configured = process.env[hfheHelperEnvVar]
  if (configured && existsSync(configured)) {
    return configured
  }
  const starts = [process.cwd(), wasmModuleDir]
  for (const start of starts) {
    let dir = start
    for (let depth = 0; depth < 10; depth += 1) {
      const match = helperCandidates(dir).find((candidate) => existsSync(candidate))
      if (match) {
        return match
      }
      const parent = path.dirname(dir)
      if (parent === dir) {
        break
      }
      dir = parent
    }
  }
  throw new Error('circle hfhe helper not found; set OCTRA_CIRCLE_HFHE_HOST')
}

const callHfheHelper = (action, payload) => {
  const helperPath = resolveHfheHelperPath()
  const raw = execFileSync(
    helperPath,
    {
      input: JSON.stringify({action, ...payload}),
      encoding: 'utf8',
      maxBuffer: 1024 * 1024,
    }
  )
  const json = JSON.parse(raw.trim() === '' ? '{}' : raw)
  if (!json.ok) {
    throw new Error(json.error || `hfhe helper failed for ${action}`)
  }
  return json.value
}

const bytesOfB64 = (value) => Uint8Array.from(Buffer.from(value, 'base64'))
const b64OfBytes = (value) => Buffer.from(value).toString('base64')

const getU32 = (bytes, offset) =>
  ((bytes[offset] << 24) >>> 0)
  | (bytes[offset + 1] << 16)
  | (bytes[offset + 2] << 8)
  | bytes[offset + 3]

const encodeU32 = (value) => Uint8Array.from([
  (value >>> 24) & 0xff,
  (value >>> 16) & 0xff,
  (value >>> 8) & 0xff,
  value & 0xff
])

const concatBytes = (...parts) => {
  const total = parts.reduce((sum, part) => sum + part.length, 0)
  const out = new Uint8Array(total)
  let offset = 0
  parts.forEach((part) => {
    out.set(part, offset)
    offset += part.length
  })
  return out
}

const h256Hex = (tag, parts) => {
  const hash = createHash('sha256')
  hash.update(tag)
  hash.update(Buffer.from([0]))
  parts.forEach((part) => {
    hash.update(Buffer.from(encodeU32(part.length)))
    hash.update(Buffer.from(part))
  })
  return hash.digest('hex')
}

const h256Raw = (tag, parts) => {
  const hash = createHash('sha256')
  hash.update(tag)
  hash.update(Buffer.from([0]))
  parts.forEach((part) => {
    hash.update(Buffer.from(encodeU32(part.length)))
    hash.update(Buffer.from(part))
  })
  return Uint8Array.from(hash.digest())
}

const encodeU64 = (value) => {
  let n = BigInt(value)
  const out = new Uint8Array(8)
  for (let i = 7; i >= 0; i -= 1) {
    out[i] = Number(n & 0xffn)
    n >>= 8n
  }
  return out
}

const base58Alphabet = '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz'

const base58Encode = (bytes) => {
  let value = 0n
  bytes.forEach((byte) => {
    value = (value << 8n) + BigInt(byte)
  })
  let out = ''
  while (value > 0n) {
    const mod = Number(value % 58n)
    out = base58Alphabet[mod] + out
    value /= 58n
  }
  bytes.some((byte) => {
    if (byte === 0) {
      out = `${base58Alphabet[0]}${out}`
      return false
    }
    return true
  })
  return out || base58Alphabet[0]
}

const extendBase58 = (value) => {
  if (value.length >= 44) {
    return value.slice(0, 44)
  }
  if (value.length === 0) {
    return '1'.repeat(44)
  }
  let out = value
  let i = 0
  while (out.length < 44) {
    out += value[i % value.length]
    i += 1
  }
  return out.slice(0, 44)
}

const circleIdOfSpawn = (payload, ownerMode, spawnNonce, payloadJson) => {
  const seed = h256Raw(
    'octra:circle_spawn_id:v1',
    [
      utf8.encode(payload.address || ''),
      utf8.encode(payload.caller || ''),
      utf8.encode(payload.tx_hash || ''),
      encodeU64(spawnNonce),
      utf8.encode(ownerMode),
      utf8.encode(payloadJson)
    ]
  )
  return `oct${extendBase58(base58Encode(seed))}`
}

const normalizeStateRef = (rawValue) => {
  const value = rawValue.trim().toLowerCase()
  if (!/^[0-9a-f]{64}$/.test(value)) {
    throw new Error('state_ref must be a 64-char hex string')
  }
  return value
}

const statePathKey = (rawStateRef) => {
  const stateRef = normalizeStateRef(rawStateRef)
  const canonicalPath = `/_state/${stateRef}`
  return h256Hex('octra:asset_state_path:v1', [utf8.encode(canonicalPath)])
}

const parseManifestResponse = (rawBytes) => {
  const jsonText = Buffer.from(rawBytes).toString('utf8')
  return JSON.parse(jsonText)
}

const responseFrame = (tag, payloadBytes = new Uint8Array()) =>
  concatBytes(utf8.encode(responseMagic), Uint8Array.from([tag]), encodeU32(payloadBytes.length), payloadBytes)

const responseNull = () => responseFrame(0)
const responseInt = (value) => responseFrame(3, utf8.encode(String(value)))
const responseString = (value) => responseFrame(4, utf8.encode(value))
const responseBool = (value) => responseFrame(value ? 2 : 1)
const responseValue = (value) => {
  if (value === null || value === undefined) {
    return responseNull()
  }
  if (typeof value === 'boolean') {
    return responseBool(value)
  }
  if (typeof value === 'number' || typeof value === 'bigint') {
    return responseInt(value)
  }
  return responseString(String(value))
}

const normalizeNonEmpty = (value, name) => {
  const normalized = String(value || '').trim()
  if (normalized.length === 0) {
    throw new Error(`${name} must not be empty`)
  }
  return normalized
}

const normalizeHex64 = (value, name) => {
  const normalized = String(value || '').trim().toLowerCase()
  if (!/^[0-9a-f]{64}$/.test(normalized)) {
    throw new Error(`${name} must be a 64-char hex string`)
  }
  return normalized
}

const encryptedAssetLocator = (kind, value) => {
  const locatorKind = normalizeNonEmpty(kind, 'locator_kind')
  const locatorValue = normalizeNonEmpty(value, 'locator_value')
  if (locatorKind === 'path') {
    return {path: locatorValue, slot_ref: null, state_ref: null}
  }
  if (locatorKind === 'slot') {
    return {path: null, slot_ref: normalizeHex64(locatorValue, 'slot_ref'), state_ref: null}
  }
  if (locatorKind === 'state') {
    return {path: null, slot_ref: null, state_ref: normalizeHex64(locatorValue, 'state_ref')}
  }
  throw new Error('locator_kind must be path, slot, or state')
}

const normalizeOptionalHex64 = (value, name) => {
  if (value === null) {
    return null
  }
  return normalizeHex64(value, name)
}

const normalizeOptionalString = (value) => {
  if (value === null) {
    return null
  }
  const normalized = String(value).trim()
  return normalized.length === 0 ? null : normalized
}

const normalizeBoolLiteral = (value) => value ? 'true' : 'false'

const parseIntParam = (params, idx, name) => {
  const raw = params[idx]
  if (typeof raw !== 'string' || raw.trim() === '') {
    throw new Error(`${name} must be an integer string`)
  }
  if (!/^-?[0-9]+$/.test(raw.trim())) {
    throw new Error(`${name} must be an integer string`)
  }
  const parsed = BigInt(raw.trim())
  return parsed
}

const parseOptionalIntParam = (params, idx, name) => {
  const raw = params[idx]
  if (raw === null) {
    return null
  }
  return parseIntParam(params, idx, name)
}

const parseStringParam = (params, idx, name) => {
  const raw = params[idx]
  if (typeof raw !== 'string') {
    throw new Error(`${name} must be a string`)
  }
  return raw
}

const parseOptionalStringParam = (params, idx) => {
  const raw = params[idx]
  if (raw === null) {
    return null
  }
  if (typeof raw !== 'string') {
    throw new Error('optional string param must be a string or null')
  }
  return raw
}

const parseBoolParam = (params, idx, name) => {
  const raw = params[idx]
  if (typeof raw !== 'boolean') {
    throw new Error(`${name} must be a bool`)
  }
  return raw
}

const storageGetUtf8 = (storage, key) => {
  const value = storage.get(key)
  return value ? Buffer.from(value).toString('utf8') : null
}

const storageSetUtf8 = (storage, key, value) => {
  storage.set(key, utf8.encode(String(value)))
}

const storageSetOptionalUtf8 = (storage, key, value) => {
  if (value === null || value === '') {
    storage.delete(key)
  } else {
    storageSetUtf8(storage, key, value)
  }
}

const storageSetBool = (storage, key, value) => {
  storageSetUtf8(storage, key, normalizeBoolLiteral(value))
}

const storageSetInt = (storage, key, value) => {
  storageSetUtf8(storage, key, String(value))
}

const storageGetInt = (storage, key) => {
  const raw = storageGetUtf8(storage, key)
  if (raw === null) {
    return null
  }
  if (!/^-?[0-9]+$/.test(raw.trim())) {
    return null
  }
  return BigInt(raw.trim())
}

const storageGetBool = (storage, key) => {
  const raw = storageGetUtf8(storage, key)
  if (raw === null) {
    return null
  }
  const normalized = raw.trim().toLowerCase()
  if (normalized === '1' || normalized === 'true' || normalized === 'yes') {
    return true
  }
  if (normalized === '0' || normalized === 'false' || normalized === 'no') {
    return false
  }
  return null
}

const stateDescriptorKey = (stateRef, suffix) => `state_descriptor:${statePathKey(stateRef)}:${suffix}`
const statePolicyKey = (stateRef, suffix) => `state_policy:${statePathKey(stateRef)}:${suffix}`
const balanceCellKey = (stateRef, suffix) => `balance_cell:${statePathKey(stateRef)}:${suffix}`
const registerCellKey = (stateRef, suffix) => `register_cell:${statePathKey(stateRef)}:${suffix}`
const balanceBindingKey = (subjectAddr, suffix) => `balance_binding:${subjectAddr}:${suffix}`
const registerBindingKey = (registerRef, suffix) => `register_binding:${registerRef}:${suffix}`
const balanceWorkflowKey = (workflowRef, suffix) => `balance_workflow:${workflowRef}:${suffix}`
const registerWorkflowKey = (workflowRef, suffix) => `register_workflow:${workflowRef}:${suffix}`
const objectBindingKey = (objectRef, suffix) => `object_binding:${objectRef}:${suffix}`
const objectMemberKey = (objectRef, memberRef, suffix) => `object_member:${objectRef}:${memberRef}:${suffix}`
const objectPolicyKey = (objectRef, suffix) => `object_policy:${objectRef}:${suffix}`
const objectTransitionKey = (transitionRef, suffix) => `object_transition:${transitionRef}:${suffix}`

const incrementStoredVersion = (storage, key) => {
  const current = storageGetInt(storage, key)
  const next = current === null ? 1n : current + 1n
  storageSetInt(storage, key, next)
  return next
}

const normalizeMemberRef = (rawValue, field) => {
  const value = normalizeNonEmpty(rawValue, field)
  if (value.includes(':')) {
    throw new Error(`${field} must not contain ':'`)
  }
  return value
}

const parseObjectMemberBundle = (bundle) => {
  if (bundle.trim().length === 0) {
    return []
  }
  return bundle.split(';').map((entry) => {
    const parts = entry.split('|')
    if (parts[0] === 'attach' && parts.length === 7) {
      return {
        operation: 'attach',
        memberRef: normalizeMemberRef(parts[1], 'member_ref'),
        stateRef: normalizeHex64(parts[2], 'state_ref'),
        memberKind: normalizeNonEmpty(parts[3], 'member_kind'),
        stateClass: normalizeNonEmpty(parts[4], 'state_class'),
        codec: normalizeNonEmpty(parts[5], 'codec'),
        status: normalizeNonEmpty(parts[6], 'status')
      }
    }
    if (parts[0] === 'detach' && parts.length === 2) {
      return {
        operation: 'detach',
        memberRef: normalizeMemberRef(parts[1], 'member_ref')
      }
    }
    throw new Error('invalid object member bundle')
  })
}

const readStateDescriptorField = (storage, stateRef, field) => {
  switch (field) {
    case 'state_class':
    case 'codec':
    case 'schema_hash':
    case 'subject_addr':
    case 'hfhe_profile':
      return storageGetUtf8(storage, stateDescriptorKey(stateRef, field))
    case 'mutable_state':
      return storageGetBool(storage, stateDescriptorKey(stateRef, field))
    default:
      throw new Error(`unsupported state descriptor field: ${field}`)
  }
}

const hfheCapabilityName = (method) => {
  switch (method) {
    case 'fhe_load_pk':
      return 'fhe_load_pk'
    case 'fhe_encrypt':
    case 'fhe_encrypt_zero':
      return 'fhe_encrypt'
    case 'fhe_decrypt':
      return 'fhe_decrypt'
    case 'fhe_add':
    case 'fhe_sub':
    case 'fhe_scale':
    case 'fhe_add_const':
    case 'fhe_sub_const':
      return 'fhe_cipher_arithmetic'
    case 'fhe_commit':
    case 'fhe_bound_commitment':
      return 'fhe_commit'
    case 'fhe_pedersen':
      return 'fhe_pedersen'
    case 'fhe_verify_zero':
      return 'fhe_verify_zero'
    case 'fhe_verify_range':
      return 'fhe_verify_range'
    case 'fhe_verify_bound':
      return 'fhe_verify_bound'
    default:
      return null
  }
}

const executeHfheInvoke = (hfheCaps, hfhePubkeys, hfheActiveKey, reqBytes) => {
  const request = parseRequestFrame(reqBytes)
  const capability = hfheCapabilityName(request.method)
  if (!capability || !hfheCaps.has(capability)) {
    throw new Error(`hfhe capability denied: ${request.method}`)
  }
  const params = request.params
  switch (request.method) {
    case 'fhe_load_pk': {
      const requestedAddr = parseStringParam(params, 0, 'requested_addr')
      const pubkeyB64 = hfhePubkeys.get(requestedAddr)
      if (!pubkeyB64) {
        throw new Error(`hfhe pubkey unavailable: ${requestedAddr}`)
      }
      return responseString(pubkeyB64)
    }
    case 'fhe_encrypt': {
      if (!hfheActiveKey) {
        throw new Error('hfhe active key unavailable')
      }
      const amount = parseIntParam(params, 0, 'amount')
      const seedB64 = parseStringParam(params, 1, 'seed_b64')
      const value = callHfheHelper('encrypt_value_seeded', {
        pubkey_b64: hfheActiveKey.pubkey_b64,
        seckey_b64: hfheActiveKey.seckey_b64,
        amount: amount.toString(),
        seed_b64: seedB64,
      })
      return responseString(String(value))
    }
    case 'fhe_encrypt_zero': {
      if (!hfheActiveKey) {
        throw new Error('hfhe active key unavailable')
      }
      const seedB64 = parseStringParam(params, 0, 'seed_b64')
      const value = callHfheHelper('encrypt_zero_seeded', {
        pubkey_b64: hfheActiveKey.pubkey_b64,
        seckey_b64: hfheActiveKey.seckey_b64,
        seed_b64: seedB64,
      })
      return responseString(String(value))
    }
    case 'fhe_decrypt': {
      if (!hfheActiveKey) {
        throw new Error('hfhe active key unavailable')
      }
      const ciphertext = parseStringParam(params, 0, 'ciphertext')
      const value = callHfheHelper('decrypt_value', {
        pubkey_b64: hfheActiveKey.pubkey_b64,
        seckey_b64: hfheActiveKey.seckey_b64,
        ciphertext,
      })
      return responseInt(String(value))
    }
    case 'fhe_add': {
      const pubkeyB64 = parseStringParam(params, 0, 'pubkey_b64')
      const lhsCiphertext = parseStringParam(params, 1, 'lhs_ciphertext')
      const rhsCiphertext = parseStringParam(params, 2, 'rhs_ciphertext')
      const value = callHfheHelper('cipher_add', {pubkey_b64: pubkeyB64, lhs_ciphertext: lhsCiphertext, rhs_ciphertext: rhsCiphertext})
      return responseString(String(value))
    }
    case 'fhe_sub': {
      const pubkeyB64 = parseStringParam(params, 0, 'pubkey_b64')
      const lhsCiphertext = parseStringParam(params, 1, 'lhs_ciphertext')
      const rhsCiphertext = parseStringParam(params, 2, 'rhs_ciphertext')
      const value = callHfheHelper('cipher_sub', {pubkey_b64: pubkeyB64, lhs_ciphertext: lhsCiphertext, rhs_ciphertext: rhsCiphertext})
      return responseString(String(value))
    }
    case 'fhe_scale': {
      const pubkeyB64 = parseStringParam(params, 0, 'pubkey_b64')
      const ciphertext = parseStringParam(params, 1, 'ciphertext')
      const factor = parseIntParam(params, 2, 'factor')
      const value = callHfheHelper('cipher_scale', {pubkey_b64: pubkeyB64, ciphertext, factor: factor.toString()})
      return responseString(String(value))
    }
    case 'fhe_add_const': {
      const pubkeyB64 = parseStringParam(params, 0, 'pubkey_b64')
      const ciphertext = parseStringParam(params, 1, 'ciphertext')
      const amount = parseIntParam(params, 2, 'amount')
      const value = callHfheHelper('cipher_add_const', {pubkey_b64: pubkeyB64, ciphertext, amount: amount.toString()})
      return responseString(String(value))
    }
    case 'fhe_sub_const': {
      const pubkeyB64 = parseStringParam(params, 0, 'pubkey_b64')
      const ciphertext = parseStringParam(params, 1, 'ciphertext')
      const amount = parseIntParam(params, 2, 'amount')
      const value = callHfheHelper('cipher_sub_const', {pubkey_b64: pubkeyB64, ciphertext, amount: amount.toString()})
      return responseString(String(value))
    }
    case 'fhe_pedersen': {
      const amount = parseIntParam(params, 0, 'amount')
      const blindingB64 = parseStringParam(params, 1, 'blinding_b64')
      const value = callHfheHelper('pedersen_commit', {amount: amount.toString(), blinding_b64: blindingB64})
      return responseString(String(value))
    }
    case 'fhe_commit': {
      const pubkeyB64 = parseStringParam(params, 0, 'pubkey_b64')
      const ciphertext = parseStringParam(params, 1, 'ciphertext')
      const value = callHfheHelper('commit_cipher', {pubkey_b64: pubkeyB64, ciphertext})
      return responseString(String(value))
    }
    case 'fhe_bound_commitment': {
      const pubkeyB64 = parseStringParam(params, 0, 'pubkey_b64')
      const ciphertext = parseStringParam(params, 1, 'ciphertext')
      const amount = parseIntParam(params, 2, 'amount')
      const value = callHfheHelper('bound_commitment', {pubkey_b64: pubkeyB64, ciphertext, amount: amount.toString()})
      return responseString(String(value))
    }
    case 'fhe_verify_zero': {
      const pubkeyB64 = parseStringParam(params, 0, 'pubkey_b64')
      const ciphertext = parseStringParam(params, 1, 'ciphertext')
      const proof = parseStringParam(params, 2, 'proof')
      const value = callHfheHelper('verify_zero', {pubkey_b64: pubkeyB64, ciphertext, proof})
      return responseBool(Boolean(value))
    }
    case 'fhe_verify_range': {
      const pubkeyB64 = parseStringParam(params, 0, 'pubkey_b64')
      const ciphertext = parseStringParam(params, 1, 'ciphertext')
      const proof = parseStringParam(params, 2, 'proof')
      const value = callHfheHelper('verify_range', {pubkey_b64: pubkeyB64, ciphertext, proof})
      return responseBool(Boolean(value))
    }
    case 'fhe_verify_bound': {
      const pubkeyB64 = parseStringParam(params, 0, 'pubkey_b64')
      const ciphertext = parseStringParam(params, 1, 'ciphertext')
      const proof = parseStringParam(params, 2, 'proof')
      const amountCommitment = parseStringParam(params, 3, 'amount_commitment')
      const value = callHfheHelper('verify_bound', {pubkey_b64: pubkeyB64, ciphertext, proof, amount_commitment: amountCommitment})
      return responseBool(Boolean(value))
    }
    default:
      throw new Error(`unsupported hfhe host method: ${request.method}`)
  }
}

const readStatePolicyField = (storage, stateRef, field) => {
  switch (field) {
    case 'delivery_key_id':
      return storageGetUtf8(storage, statePolicyKey(stateRef, field))
    case 'activate_after_epoch':
    case 'expire_after_epoch':
      return storageGetInt(storage, statePolicyKey(stateRef, field))
    case 'tombstone':
    case 'revoked':
      return storageGetBool(storage, statePolicyKey(stateRef, field))
    default:
      throw new Error(`unsupported state policy field: ${field}`)
  }
}

const readBalanceCellField = (storage, stateRef, field) => {
  switch (field) {
    case 'ciphertext_commitment':
    case 'amount_commitment':
    case 'proof_kind':
    case 'proof_receipt_hash':
      return storageGetUtf8(storage, balanceCellKey(stateRef, field))
    default:
      throw new Error(`unsupported balance cell field: ${field}`)
  }
}

const readRegisterCellField = (storage, stateRef, field) => {
  switch (field) {
    case 'ciphertext_commitment':
    case 'proof_kind':
    case 'proof_receipt_hash':
      return storageGetUtf8(storage, registerCellKey(stateRef, field))
    default:
      throw new Error(`unsupported register cell field: ${field}`)
  }
}

const readBalanceBindingField = (storage, subjectAddr, field) => {
  switch (field) {
    case 'current_state_ref':
    case 'status':
    case 'last_workflow_ref':
      return storageGetUtf8(storage, balanceBindingKey(subjectAddr, field))
    case 'version':
      return storageGetInt(storage, balanceBindingKey(subjectAddr, field))
    default:
      throw new Error(`unsupported balance binding field: ${field}`)
  }
}

const readRegisterBindingField = (storage, registerRef, field) => {
  switch (field) {
    case 'current_state_ref':
    case 'status':
    case 'last_workflow_ref':
      return storageGetUtf8(storage, registerBindingKey(registerRef, field))
    case 'version':
      return storageGetInt(storage, registerBindingKey(registerRef, field))
    default:
      throw new Error(`unsupported register binding field: ${field}`)
  }
}

const readBalanceWorkflowField = (storage, workflowRef, field) => {
  switch (field) {
    case 'flow_kind':
    case 'debit_subject_addr':
    case 'credit_subject_addr':
    case 'debit_state_ref':
    case 'credit_state_ref':
    case 'amount_commitment':
    case 'proof_kind':
    case 'proof_receipt_hash':
    case 'status':
    case 'intent_id':
      return storageGetUtf8(storage, balanceWorkflowKey(workflowRef, field))
    default:
      throw new Error(`unsupported balance workflow field: ${field}`)
  }
}

const readRegisterWorkflowField = (storage, workflowRef, field) => {
  switch (field) {
    case 'register_ref':
    case 'previous_state_ref':
    case 'next_state_ref':
    case 'workflow_kind':
    case 'proof_kind':
    case 'proof_receipt_hash':
    case 'status':
    case 'intent_id':
      return storageGetUtf8(storage, registerWorkflowKey(workflowRef, field))
    default:
      throw new Error(`unsupported register workflow field: ${field}`)
  }
}

const readObjectBindingField = (storage, objectRef, field) => {
  switch (field) {
    case 'current_state_ref':
    case 'status':
    case 'last_transition_ref':
      return storageGetUtf8(storage, objectBindingKey(objectRef, field))
    case 'version':
      return storageGetInt(storage, objectBindingKey(objectRef, field))
    default:
      throw new Error(`unsupported object binding field: ${field}`)
  }
}

const readObjectMemberField = (storage, objectRef, memberRef, field) => {
  switch (field) {
    case 'state_ref':
    case 'member_kind':
    case 'state_class':
    case 'codec':
    case 'status':
      return storageGetUtf8(storage, objectMemberKey(objectRef, memberRef, field))
    default:
      throw new Error(`unsupported object member field: ${field}`)
  }
}

const readObjectPolicyField = (storage, objectRef, field) => {
  switch (field) {
    case 'delivery_key_id':
    case 'transition_mode':
    case 'required_proof_kind':
      return storageGetUtf8(storage, objectPolicyKey(objectRef, field))
    case 'activate_after_epoch':
    case 'expire_after_epoch':
    case 'member_quorum':
      return storageGetInt(storage, objectPolicyKey(objectRef, field))
    case 'tombstone':
    case 'revoked':
    case 'allow_detach':
    case 'allow_root_state_rotation':
      return storageGetBool(storage, objectPolicyKey(objectRef, field))
    default:
      throw new Error(`unsupported object policy field: ${field}`)
  }
}

const executeCircleInvoke = (storage, payload, spawns, assets, encryptedAssets, reqBytes) => {
  const request = parseRequestFrame(reqBytes)
  const params = request.params
  switch (request.method) {
    case 'circle_spawn': {
      if (payload.is_view) {
        throw new Error('circle spawn disabled in view')
      }
      if (spawns.length >= 4) {
        throw new Error('circle spawn cap exceeded')
      }
      const payloadJson = normalizeNonEmpty(parseStringParam(params, 0, 'payload_json'), 'payload_json')
      if (utf8.encode(payloadJson).length > maxSpawnPayloadJsonBytes) {
        throw new Error('circle spawn payload exceeds max size')
      }
      JSON.parse(payloadJson)
      const ownerMode = normalizeNonEmpty(parseStringParam(params, 1, 'owner_mode'), 'owner_mode')
      if (ownerMode !== 'caller' && ownerMode !== 'parent') {
        throw new Error('owner_mode must be caller or parent')
      }
      const spawnNonce = spawns.length
      const circleId = circleIdOfSpawn(payload, ownerMode, spawnNonce, payloadJson)
      spawns.push({
        circle_id: circleId,
        owner_mode: ownerMode,
        spawn_nonce: spawnNonce,
        payload_json: payloadJson
      })
      return responseString(circleId)
    }
    case 'circle_asset_put': {
      if (payload.is_view) {
        throw new Error('circle asset effects disabled in view')
      }
      if (assets.length + encryptedAssets.length >= 4) {
        throw new Error('circle asset effect cap exceeded')
      }
      const circleId = normalizeNonEmpty(parseStringParam(params, 0, 'circle_id'), 'circle_id')
      const path = normalizeNonEmpty(parseStringParam(params, 1, 'path'), 'path')
      const contentType = normalizeNonEmpty(parseStringParam(params, 2, 'content_type'), 'content_type')
      const bodyB64 = normalizeNonEmpty(parseStringParam(params, 3, 'body_b64'), 'body_b64')
      const encoding = normalizeOptionalString(parseOptionalStringParam(params, 4))
      if (bodyB64.length > maxAssetEffectBodyB64Bytes) {
        throw new Error('circle asset effect body exceeds max encoded size')
      }
      assets.push({
        circle_id: circleId,
        path,
        content_type: contentType,
        encoding,
        body_b64: bodyB64
      })
      return responseBool(true)
    }
    case 'circle_asset_put_encrypted': {
      if (payload.is_view) {
        throw new Error('circle encrypted asset effects disabled in view')
      }
      if (assets.length + encryptedAssets.length >= 4) {
        throw new Error('circle asset effect cap exceeded')
      }
      const circleId = normalizeNonEmpty(parseStringParam(params, 0, 'circle_id'), 'circle_id')
      const path = normalizeNonEmpty(parseStringParam(params, 1, 'path'), 'path')
      const contentType = normalizeNonEmpty(parseStringParam(params, 2, 'content_type'), 'content_type')
      const ciphertextB64 = normalizeNonEmpty(parseStringParam(params, 3, 'ciphertext_b64'), 'ciphertext_b64')
      const keyId = normalizeNonEmpty(parseStringParam(params, 4, 'key_id'), 'key_id')
      const plaintextHash = normalizeHex64(parseStringParam(params, 5, 'plaintext_hash'), 'plaintext_hash')
      const encoding = normalizeOptionalString(parseOptionalStringParam(params, 6))
      const paddingClass = normalizeOptionalString(parseOptionalStringParam(params, 7))
      const activateAfter = parseOptionalIntParam(params, 8, 'activate_after_epoch')
      const expireAfter = parseOptionalIntParam(params, 9, 'expire_after_epoch')
      const metadataMode = normalizeOptionalString(parseOptionalStringParam(params, 10))
      if (ciphertextB64.length > maxAssetEffectBodyB64Bytes) {
        throw new Error('circle encrypted asset effect body exceeds max encoded size')
      }
      if (metadataMode !== null && metadataMode !== 'reveal' && metadataMode !== 'opaque') {
        throw new Error('metadata_mode must be reveal or opaque')
      }
      encryptedAssets.push({
        circle_id: circleId,
        path,
        slot_ref: null,
        state_ref: null,
        content_type: contentType,
        encoding,
        key_id: keyId,
        plaintext_hash: plaintextHash,
        padding_class: paddingClass,
        activate_after_epoch: activateAfter === null ? null : String(activateAfter),
        expire_after_epoch: expireAfter === null ? null : String(expireAfter),
        metadata_mode: metadataMode,
        ciphertext_b64: ciphertextB64
      })
      return responseBool(true)
    }
    case 'circle_asset_put_encrypted_at': {
      if (payload.is_view) {
        throw new Error('circle encrypted asset effects disabled in view')
      }
      if (assets.length + encryptedAssets.length >= 4) {
        throw new Error('circle asset effect cap exceeded')
      }
      const circleId = normalizeNonEmpty(parseStringParam(params, 0, 'circle_id'), 'circle_id')
      const locator = encryptedAssetLocator(parseStringParam(params, 1, 'locator_kind'), parseStringParam(params, 2, 'locator_value'))
      const contentType = normalizeNonEmpty(parseStringParam(params, 3, 'content_type'), 'content_type')
      const ciphertextB64 = normalizeNonEmpty(parseStringParam(params, 4, 'ciphertext_b64'), 'ciphertext_b64')
      const keyId = normalizeNonEmpty(parseStringParam(params, 5, 'key_id'), 'key_id')
      const plaintextHash = normalizeHex64(parseStringParam(params, 6, 'plaintext_hash'), 'plaintext_hash')
      const encoding = normalizeOptionalString(parseOptionalStringParam(params, 7))
      const paddingClass = normalizeOptionalString(parseOptionalStringParam(params, 8))
      const activateAfter = parseOptionalIntParam(params, 9, 'activate_after_epoch')
      const expireAfter = parseOptionalIntParam(params, 10, 'expire_after_epoch')
      const metadataMode = normalizeOptionalString(parseOptionalStringParam(params, 11))
      if (ciphertextB64.length > maxAssetEffectBodyB64Bytes) {
        throw new Error('circle encrypted asset effect body exceeds max encoded size')
      }
      if (metadataMode !== null && metadataMode !== 'reveal' && metadataMode !== 'opaque') {
        throw new Error('metadata_mode must be reveal or opaque')
      }
      encryptedAssets.push({
        circle_id: circleId,
        path: locator.path,
        slot_ref: locator.slot_ref,
        state_ref: locator.state_ref,
        content_type: contentType,
        encoding,
        key_id: keyId,
        plaintext_hash: plaintextHash,
        padding_class: paddingClass,
        activate_after_epoch: activateAfter === null ? null : String(activateAfter),
        expire_after_epoch: expireAfter === null ? null : String(expireAfter),
        metadata_mode: metadataMode,
        ciphertext_b64: ciphertextB64
      })
      return responseBool(true)
    }
    case 'state_describe': {
      const stateRef = normalizeHex64(parseStringParam(params, 0, 'state_ref'), 'state_ref')
      const stateClass = normalizeNonEmpty(parseStringParam(params, 1, 'state_class'), 'state_class')
      const codec = normalizeNonEmpty(parseStringParam(params, 2, 'codec'), 'codec')
      const schemaHash = normalizeOptionalString(parseOptionalStringParam(params, 3))
      const subjectAddr = normalizeOptionalString(parseOptionalStringParam(params, 4))
      const hfheProfile = normalizeNonEmpty(parseStringParam(params, 5, 'hfhe_profile'), 'hfhe_profile')
      const mutableState = parseBoolParam(params, 6, 'mutable_state')
      storageSetUtf8(storage, stateDescriptorKey(stateRef, 'state_class'), stateClass)
      storageSetUtf8(storage, stateDescriptorKey(stateRef, 'codec'), codec)
      storageSetOptionalUtf8(storage, stateDescriptorKey(stateRef, 'schema_hash'), schemaHash)
      storageSetOptionalUtf8(storage, stateDescriptorKey(stateRef, 'subject_addr'), subjectAddr)
      storageSetUtf8(storage, stateDescriptorKey(stateRef, 'hfhe_profile'), hfheProfile)
      storageSetBool(storage, stateDescriptorKey(stateRef, 'mutable_state'), mutableState)
      return responseBool(true)
    }
    case 'state_publish': {
      const stateRef = normalizeHex64(parseStringParam(params, 0, 'state_ref'), 'state_ref')
      const deliveryKeyId = normalizeOptionalString(parseOptionalStringParam(params, 1))
      const activateAfter = parseIntParam(params, 2, 'activate_after_epoch')
      const expireAfter = parseIntParam(params, 3, 'expire_after_epoch')
      storageSetOptionalUtf8(storage, statePolicyKey(stateRef, 'delivery_key_id'), deliveryKeyId)
      storageSetInt(storage, statePolicyKey(stateRef, 'activate_after_epoch'), activateAfter)
      storageSetInt(storage, statePolicyKey(stateRef, 'expire_after_epoch'), expireAfter)
      storageSetBool(storage, statePolicyKey(stateRef, 'tombstone'), false)
      storageSetBool(storage, statePolicyKey(stateRef, 'revoked'), false)
      return responseBool(true)
    }
    case 'state_release': {
      const stateRef = normalizeHex64(parseStringParam(params, 0, 'state_ref'), 'state_ref')
      storage.delete(statePolicyKey(stateRef, 'activate_after_epoch'))
      storage.delete(statePolicyKey(stateRef, 'expire_after_epoch'))
      storageSetBool(storage, statePolicyKey(stateRef, 'tombstone'), false)
      storageSetBool(storage, statePolicyKey(stateRef, 'revoked'), false)
      return responseBool(true)
    }
    case 'state_retire': {
      const stateRef = normalizeHex64(parseStringParam(params, 0, 'state_ref'), 'state_ref')
      const expireAfter = parseIntParam(params, 1, 'expire_after_epoch')
      storageSetInt(storage, statePolicyKey(stateRef, 'expire_after_epoch'), expireAfter)
      return responseBool(true)
    }
    case 'state_tombstone_apply': {
      const stateRef = normalizeHex64(parseStringParam(params, 0, 'state_ref'), 'state_ref')
      storageSetBool(storage, statePolicyKey(stateRef, 'tombstone'), true)
      return responseBool(true)
    }
    case 'state_restore': {
      const stateRef = normalizeHex64(parseStringParam(params, 0, 'state_ref'), 'state_ref')
      storageSetBool(storage, statePolicyKey(stateRef, 'tombstone'), false)
      return responseBool(true)
    }
    case 'state_revoke_apply': {
      const stateRef = normalizeHex64(parseStringParam(params, 0, 'state_ref'), 'state_ref')
      storageSetBool(storage, statePolicyKey(stateRef, 'revoked'), true)
      return responseBool(true)
    }
    case 'state_reinstate': {
      const stateRef = normalizeHex64(parseStringParam(params, 0, 'state_ref'), 'state_ref')
      storageSetBool(storage, statePolicyKey(stateRef, 'revoked'), false)
      return responseBool(true)
    }
    case 'state_descriptor_get': {
      const stateRef = normalizeHex64(parseStringParam(params, 0, 'state_ref'), 'state_ref')
      const field = normalizeNonEmpty(parseStringParam(params, 1, 'field'), 'field')
      return responseValue(readStateDescriptorField(storage, stateRef, field))
    }
    case 'state_policy_get': {
      const stateRef = normalizeHex64(parseStringParam(params, 0, 'state_ref'), 'state_ref')
      const field = normalizeNonEmpty(parseStringParam(params, 1, 'field'), 'field')
      return responseValue(readStatePolicyField(storage, stateRef, field))
    }
    case 'balance_cell_materialize': {
      const stateRef = normalizeHex64(parseStringParam(params, 0, 'state_ref'), 'state_ref')
      storageSetUtf8(storage, balanceCellKey(stateRef, 'ciphertext_commitment'), normalizeNonEmpty(parseStringParam(params, 1, 'ciphertext_commitment'), 'ciphertext_commitment'))
      storageSetUtf8(storage, balanceCellKey(stateRef, 'amount_commitment'), normalizeNonEmpty(parseStringParam(params, 2, 'amount_commitment'), 'amount_commitment'))
      storageSetUtf8(storage, balanceCellKey(stateRef, 'proof_kind'), normalizeNonEmpty(parseStringParam(params, 3, 'proof_kind'), 'proof_kind'))
      storageSetOptionalUtf8(storage, balanceCellKey(stateRef, 'proof_receipt_hash'), normalizeOptionalHex64(parseOptionalStringParam(params, 4), 'proof_receipt_hash'))
      return responseBool(true)
    }
    case 'balance_cell_get': {
      const stateRef = normalizeHex64(parseStringParam(params, 0, 'state_ref'), 'state_ref')
      const field = normalizeNonEmpty(parseStringParam(params, 1, 'field'), 'field')
      return responseValue(readBalanceCellField(storage, stateRef, field))
    }
    case 'register_cell_materialize': {
      const stateRef = normalizeHex64(parseStringParam(params, 0, 'state_ref'), 'state_ref')
      storageSetUtf8(storage, registerCellKey(stateRef, 'ciphertext_commitment'), normalizeNonEmpty(parseStringParam(params, 1, 'ciphertext_commitment'), 'ciphertext_commitment'))
      storageSetUtf8(storage, registerCellKey(stateRef, 'proof_kind'), normalizeNonEmpty(parseStringParam(params, 2, 'proof_kind'), 'proof_kind'))
      storageSetOptionalUtf8(storage, registerCellKey(stateRef, 'proof_receipt_hash'), normalizeOptionalHex64(parseOptionalStringParam(params, 3), 'proof_receipt_hash'))
      return responseBool(true)
    }
    case 'register_cell_get': {
      const stateRef = normalizeHex64(parseStringParam(params, 0, 'state_ref'), 'state_ref')
      const field = normalizeNonEmpty(parseStringParam(params, 1, 'field'), 'field')
      return responseValue(readRegisterCellField(storage, stateRef, field))
    }
    case 'balance_binding_bind': {
      const subjectAddr = normalizeNonEmpty(parseStringParam(params, 0, 'subject_addr'), 'subject_addr')
      storageSetUtf8(storage, balanceBindingKey(subjectAddr, 'current_state_ref'), normalizeHex64(parseStringParam(params, 1, 'state_ref'), 'state_ref'))
      const version = incrementStoredVersion(storage, balanceBindingKey(subjectAddr, 'version'))
      storageSetUtf8(storage, balanceBindingKey(subjectAddr, 'status'), normalizeNonEmpty(parseStringParam(params, 3, 'status'), 'status'))
      storageSetUtf8(storage, balanceBindingKey(subjectAddr, 'last_workflow_ref'), normalizeHex64(parseStringParam(params, 2, 'workflow_ref'), 'workflow_ref'))
      return responseInt(version)
    }
    case 'balance_binding_get': {
      const subjectAddr = normalizeNonEmpty(parseStringParam(params, 0, 'subject_addr'), 'subject_addr')
      const field = normalizeNonEmpty(parseStringParam(params, 1, 'field'), 'field')
      return responseValue(readBalanceBindingField(storage, subjectAddr, field))
    }
    case 'register_binding_bind': {
      const registerRef = normalizeHex64(parseStringParam(params, 0, 'register_ref'), 'register_ref')
      storageSetUtf8(storage, registerBindingKey(registerRef, 'current_state_ref'), normalizeHex64(parseStringParam(params, 1, 'state_ref'), 'state_ref'))
      const version = incrementStoredVersion(storage, registerBindingKey(registerRef, 'version'))
      storageSetUtf8(storage, registerBindingKey(registerRef, 'status'), normalizeNonEmpty(parseStringParam(params, 3, 'status'), 'status'))
      storageSetUtf8(storage, registerBindingKey(registerRef, 'last_workflow_ref'), normalizeHex64(parseStringParam(params, 2, 'workflow_ref'), 'workflow_ref'))
      return responseInt(version)
    }
    case 'register_binding_get': {
      const registerRef = normalizeHex64(parseStringParam(params, 0, 'register_ref'), 'register_ref')
      const field = normalizeNonEmpty(parseStringParam(params, 1, 'field'), 'field')
      return responseValue(readRegisterBindingField(storage, registerRef, field))
    }
    case 'balance_workflow_record': {
      const workflowRef = normalizeHex64(parseStringParam(params, 0, 'workflow_ref'), 'workflow_ref')
      const fields = [
        ['flow_kind', parseStringParam(params, 1, 'flow_kind')],
        ['debit_subject_addr', parseStringParam(params, 2, 'debit_subject_addr')],
        ['credit_subject_addr', parseStringParam(params, 3, 'credit_subject_addr')],
        ['debit_state_ref', parseStringParam(params, 4, 'debit_state_ref')],
        ['credit_state_ref', parseStringParam(params, 5, 'credit_state_ref')],
        ['amount_commitment', parseStringParam(params, 6, 'amount_commitment')],
        ['proof_kind', parseStringParam(params, 7, 'proof_kind')],
        ['proof_receipt_hash', parseStringParam(params, 8, 'proof_receipt_hash')],
        ['status', parseStringParam(params, 9, 'status')],
        ['intent_id', parseStringParam(params, 10, 'intent_id')],
      ]
      fields.forEach(([suffix, value]) => {
        storageSetUtf8(storage, balanceWorkflowKey(workflowRef, suffix), normalizeNonEmpty(value, suffix))
      })
      return responseBool(true)
    }
    case 'balance_workflow_get': {
      const workflowRef = normalizeHex64(parseStringParam(params, 0, 'workflow_ref'), 'workflow_ref')
      const field = normalizeNonEmpty(parseStringParam(params, 1, 'field'), 'field')
      return responseValue(readBalanceWorkflowField(storage, workflowRef, field))
    }
    case 'register_workflow_record': {
      const workflowRef = normalizeHex64(parseStringParam(params, 0, 'workflow_ref'), 'workflow_ref')
      const fields = [
        ['register_ref', parseStringParam(params, 1, 'register_ref')],
        ['previous_state_ref', parseStringParam(params, 2, 'previous_state_ref')],
        ['next_state_ref', parseStringParam(params, 3, 'next_state_ref')],
        ['workflow_kind', parseStringParam(params, 4, 'workflow_kind')],
        ['proof_kind', parseStringParam(params, 5, 'proof_kind')],
        ['proof_receipt_hash', parseStringParam(params, 6, 'proof_receipt_hash')],
        ['status', parseStringParam(params, 7, 'status')],
        ['intent_id', parseStringParam(params, 8, 'intent_id')],
      ]
      fields.forEach(([suffix, value]) => {
        storageSetUtf8(storage, registerWorkflowKey(workflowRef, suffix), normalizeNonEmpty(value, suffix))
      })
      return responseBool(true)
    }
    case 'register_workflow_get': {
      const workflowRef = normalizeHex64(parseStringParam(params, 0, 'workflow_ref'), 'workflow_ref')
      const field = normalizeNonEmpty(parseStringParam(params, 1, 'field'), 'field')
      return responseValue(readRegisterWorkflowField(storage, workflowRef, field))
    }
    case 'object_bind': {
      const objectRef = normalizeHex64(parseStringParam(params, 0, 'object_ref'), 'object_ref')
      storageSetUtf8(storage, objectBindingKey(objectRef, 'current_state_ref'), normalizeHex64(parseStringParam(params, 1, 'state_ref'), 'state_ref'))
      const version = incrementStoredVersion(storage, objectBindingKey(objectRef, 'version'))
      storageSetUtf8(storage, objectBindingKey(objectRef, 'last_transition_ref'), normalizeHex64(parseStringParam(params, 2, 'transition_ref'), 'transition_ref'))
      storageSetUtf8(storage, objectBindingKey(objectRef, 'status'), normalizeNonEmpty(parseStringParam(params, 3, 'status'), 'status'))
      return responseInt(version)
    }
    case 'object_binding_get': {
      const objectRef = normalizeHex64(parseStringParam(params, 0, 'object_ref'), 'object_ref')
      const field = normalizeNonEmpty(parseStringParam(params, 1, 'field'), 'field')
      return responseValue(readObjectBindingField(storage, objectRef, field))
    }
    case 'object_member_attach': {
      const objectRef = normalizeHex64(parseStringParam(params, 0, 'object_ref'), 'object_ref')
      const memberRef = normalizeNonEmpty(parseStringParam(params, 1, 'member_ref'), 'member_ref')
      storageSetUtf8(storage, objectMemberKey(objectRef, memberRef, 'state_ref'), normalizeHex64(parseStringParam(params, 2, 'state_ref'), 'state_ref'))
      storageSetUtf8(storage, objectMemberKey(objectRef, memberRef, 'member_kind'), normalizeNonEmpty(parseStringParam(params, 3, 'member_kind'), 'member_kind'))
      storageSetUtf8(storage, objectMemberKey(objectRef, memberRef, 'state_class'), normalizeNonEmpty(parseStringParam(params, 4, 'state_class'), 'state_class'))
      storageSetUtf8(storage, objectMemberKey(objectRef, memberRef, 'codec'), normalizeNonEmpty(parseStringParam(params, 5, 'codec'), 'codec'))
      storageSetUtf8(storage, objectMemberKey(objectRef, memberRef, 'status'), normalizeNonEmpty(parseStringParam(params, 6, 'status'), 'status'))
      return responseBool(true)
    }
    case 'object_member_detach': {
      const objectRef = normalizeHex64(parseStringParam(params, 0, 'object_ref'), 'object_ref')
      const memberRef = normalizeNonEmpty(parseStringParam(params, 1, 'member_ref'), 'member_ref')
      storage.delete(objectMemberKey(objectRef, memberRef, 'state_ref'))
      storage.delete(objectMemberKey(objectRef, memberRef, 'member_kind'))
      storage.delete(objectMemberKey(objectRef, memberRef, 'state_class'))
      storage.delete(objectMemberKey(objectRef, memberRef, 'codec'))
      storage.delete(objectMemberKey(objectRef, memberRef, 'status'))
      return responseBool(true)
    }
    case 'object_member_get': {
      const objectRef = normalizeHex64(parseStringParam(params, 0, 'object_ref'), 'object_ref')
      const memberRef = normalizeNonEmpty(parseStringParam(params, 1, 'member_ref'), 'member_ref')
      const field = normalizeNonEmpty(parseStringParam(params, 2, 'field'), 'field')
      return responseValue(readObjectMemberField(storage, objectRef, memberRef, field))
    }
    case 'object_policy_define': {
      const objectRef = normalizeHex64(parseStringParam(params, 0, 'object_ref'), 'object_ref')
      storageSetOptionalUtf8(storage, objectPolicyKey(objectRef, 'delivery_key_id'), normalizeOptionalString(parseOptionalStringParam(params, 1)))
      storageSetInt(storage, objectPolicyKey(objectRef, 'activate_after_epoch'), parseIntParam(params, 2, 'activate_after_epoch'))
      storageSetInt(storage, objectPolicyKey(objectRef, 'expire_after_epoch'), parseIntParam(params, 3, 'expire_after_epoch'))
      storageSetUtf8(storage, objectPolicyKey(objectRef, 'transition_mode'), normalizeNonEmpty(parseStringParam(params, 4, 'transition_mode'), 'transition_mode'))
      storageSetUtf8(storage, objectPolicyKey(objectRef, 'required_proof_kind'), normalizeNonEmpty(parseStringParam(params, 5, 'required_proof_kind'), 'required_proof_kind'))
      storageSetInt(storage, objectPolicyKey(objectRef, 'member_quorum'), parseIntParam(params, 6, 'member_quorum'))
      storageSetBool(storage, objectPolicyKey(objectRef, 'allow_detach'), parseBoolParam(params, 7, 'allow_detach'))
      storageSetBool(storage, objectPolicyKey(objectRef, 'allow_root_state_rotation'), parseBoolParam(params, 8, 'allow_root_state_rotation'))
      storageSetBool(storage, objectPolicyKey(objectRef, 'tombstone'), false)
      storageSetBool(storage, objectPolicyKey(objectRef, 'revoked'), false)
      return responseBool(true)
    }
    case 'object_policy_release': {
      const objectRef = normalizeHex64(parseStringParam(params, 0, 'object_ref'), 'object_ref')
      storage.delete(objectPolicyKey(objectRef, 'delivery_key_id'))
      storage.delete(objectPolicyKey(objectRef, 'activate_after_epoch'))
      storage.delete(objectPolicyKey(objectRef, 'expire_after_epoch'))
      storageSetBool(storage, objectPolicyKey(objectRef, 'tombstone'), false)
      storageSetBool(storage, objectPolicyKey(objectRef, 'revoked'), false)
      return responseBool(true)
    }
    case 'object_policy_retire': {
      const objectRef = normalizeHex64(parseStringParam(params, 0, 'object_ref'), 'object_ref')
      storageSetInt(storage, objectPolicyKey(objectRef, 'expire_after_epoch'), parseIntParam(params, 1, 'expire_after_epoch'))
      return responseBool(true)
    }
    case 'object_policy_tombstone': {
      const objectRef = normalizeHex64(parseStringParam(params, 0, 'object_ref'), 'object_ref')
      storageSetBool(storage, objectPolicyKey(objectRef, 'tombstone'), true)
      return responseBool(true)
    }
    case 'object_policy_restore': {
      const objectRef = normalizeHex64(parseStringParam(params, 0, 'object_ref'), 'object_ref')
      storageSetBool(storage, objectPolicyKey(objectRef, 'tombstone'), false)
      return responseBool(true)
    }
    case 'object_policy_revoke': {
      const objectRef = normalizeHex64(parseStringParam(params, 0, 'object_ref'), 'object_ref')
      storageSetBool(storage, objectPolicyKey(objectRef, 'revoked'), true)
      return responseBool(true)
    }
    case 'object_policy_reinstate': {
      const objectRef = normalizeHex64(parseStringParam(params, 0, 'object_ref'), 'object_ref')
      storageSetBool(storage, objectPolicyKey(objectRef, 'revoked'), false)
      return responseBool(true)
    }
    case 'object_policy_get': {
      const objectRef = normalizeHex64(parseStringParam(params, 0, 'object_ref'), 'object_ref')
      const field = normalizeNonEmpty(parseStringParam(params, 1, 'field'), 'field')
      return responseValue(readObjectPolicyField(storage, objectRef, field))
    }
    case 'object_transition_record': {
      const transitionRef = normalizeHex64(parseStringParam(params, 0, 'transition_ref'), 'transition_ref')
      storageSetUtf8(storage, objectTransitionKey(transitionRef, 'object_ref'), normalizeHex64(parseStringParam(params, 1, 'object_ref'), 'object_ref'))
      storageSetUtf8(storage, objectTransitionKey(transitionRef, 'previous_state_ref'), normalizeHex64(parseStringParam(params, 2, 'previous_state_ref'), 'previous_state_ref'))
      storageSetUtf8(storage, objectTransitionKey(transitionRef, 'next_state_ref'), normalizeHex64(parseStringParam(params, 3, 'next_state_ref'), 'next_state_ref'))
      storageSetUtf8(storage, objectTransitionKey(transitionRef, 'touched_members_hash'), normalizeHex64(parseStringParam(params, 4, 'touched_members_hash'), 'touched_members_hash'))
      storageSetUtf8(storage, objectTransitionKey(transitionRef, 'proof_kind'), normalizeNonEmpty(parseStringParam(params, 5, 'proof_kind'), 'proof_kind'))
      storageSetUtf8(storage, objectTransitionKey(transitionRef, 'proof_receipt_hash'), normalizeHex64(parseStringParam(params, 6, 'proof_receipt_hash'), 'proof_receipt_hash'))
      storageSetUtf8(storage, objectTransitionKey(transitionRef, 'status'), normalizeNonEmpty(parseStringParam(params, 7, 'status'), 'status'))
      storageSetUtf8(storage, objectTransitionKey(transitionRef, 'intent_id'), normalizeHex64(parseStringParam(params, 8, 'intent_id'), 'intent_id'))
      return responseBool(true)
    }
    case 'object_transition_apply': {
      const transitionRef = normalizeHex64(parseStringParam(params, 0, 'transition_ref'), 'transition_ref')
      const objectRef = normalizeHex64(parseStringParam(params, 1, 'object_ref'), 'object_ref')
      const previousStateRef = normalizeHex64(parseStringParam(params, 2, 'previous_state_ref'), 'previous_state_ref')
      const nextStateRef = normalizeHex64(parseStringParam(params, 3, 'next_state_ref'), 'next_state_ref')
      const memberBundle = parseStringParam(params, 4, 'member_bundle')
      const deltas = parseObjectMemberBundle(memberBundle)
      const touchedMembersHash = normalizeHex64(parseStringParam(params, 5, 'touched_members_hash'), 'touched_members_hash')
      const computedHash = createHash('sha256').update(memberBundle).digest('hex')
      if (computedHash !== touchedMembersHash) {
        throw new Error('object transition touched_members_hash mismatch')
      }
      storageSetUtf8(storage, objectTransitionKey(transitionRef, 'object_ref'), objectRef)
      storageSetUtf8(storage, objectTransitionKey(transitionRef, 'previous_state_ref'), previousStateRef)
      storageSetUtf8(storage, objectTransitionKey(transitionRef, 'next_state_ref'), nextStateRef)
      storageSetUtf8(storage, objectTransitionKey(transitionRef, 'touched_members_hash'), touchedMembersHash)
      storageSetUtf8(storage, objectTransitionKey(transitionRef, 'proof_kind'), normalizeNonEmpty(parseStringParam(params, 6, 'proof_kind'), 'proof_kind'))
      storageSetUtf8(storage, objectTransitionKey(transitionRef, 'proof_receipt_hash'), normalizeHex64(parseStringParam(params, 7, 'proof_receipt_hash'), 'proof_receipt_hash'))
      const status = normalizeNonEmpty(parseStringParam(params, 8, 'status'), 'status')
      storageSetUtf8(storage, objectTransitionKey(transitionRef, 'status'), status)
      storageSetUtf8(storage, objectTransitionKey(transitionRef, 'intent_id'), normalizeHex64(parseStringParam(params, 9, 'intent_id'), 'intent_id'))
      storageSetUtf8(storage, objectBindingKey(objectRef, 'current_state_ref'), nextStateRef)
      const version = incrementStoredVersion(storage, objectBindingKey(objectRef, 'version'))
      storageSetUtf8(storage, objectBindingKey(objectRef, 'last_transition_ref'), transitionRef)
      storageSetUtf8(storage, objectBindingKey(objectRef, 'status'), status)
      deltas.forEach((delta) => {
        if (delta.operation === 'attach') {
          storageSetUtf8(storage, objectMemberKey(objectRef, delta.memberRef, 'state_ref'), delta.stateRef)
          storageSetUtf8(storage, objectMemberKey(objectRef, delta.memberRef, 'member_kind'), delta.memberKind)
          storageSetUtf8(storage, objectMemberKey(objectRef, delta.memberRef, 'state_class'), delta.stateClass)
          storageSetUtf8(storage, objectMemberKey(objectRef, delta.memberRef, 'codec'), delta.codec)
          storageSetUtf8(storage, objectMemberKey(objectRef, delta.memberRef, 'status'), delta.status)
        } else {
          storage.delete(objectMemberKey(objectRef, delta.memberRef, 'state_ref'))
          storage.delete(objectMemberKey(objectRef, delta.memberRef, 'member_kind'))
          storage.delete(objectMemberKey(objectRef, delta.memberRef, 'state_class'))
          storage.delete(objectMemberKey(objectRef, delta.memberRef, 'codec'))
          storage.delete(objectMemberKey(objectRef, delta.memberRef, 'status'))
        }
      })
      return responseInt(version)
    }
    default:
      throw new Error(`unsupported circle host method: ${request.method}`)
  }
}

const parseRequestFrame = (rawBytes) => {
  if (rawBytes.length < requestMagic.length + 4) {
    throw new Error('request too short')
  }
  const magic = Buffer.from(rawBytes.slice(0, requestMagic.length)).toString('utf8')
  if (magic !== requestMagic) {
    throw new Error('invalid request magic')
  }
  let offset = requestMagic.length
  const methodLen = (rawBytes[offset] << 8) | rawBytes[offset + 1]
  offset += 2
  const method = Buffer.from(rawBytes.slice(offset, offset + methodLen)).toString('utf8')
  offset += methodLen
  const paramCount = (rawBytes[offset] << 8) | rawBytes[offset + 1]
  offset += 2
  const params = []
  for (let i = 0; i < paramCount; i += 1) {
    const tag = rawBytes[offset]
    offset += 1
    const size = getU32(rawBytes, offset)
    offset += 4
    const payload = rawBytes.slice(offset, offset + size)
    offset += size
    switch (tag) {
      case 0:
        params.push(null)
        break
      case 1:
        params.push(false)
        break
      case 2:
        params.push(true)
        break
      case 3:
        params.push(Buffer.from(payload).toString('utf8'))
        break
      case 4:
        params.push(Buffer.from(payload).toString('utf8'))
        break
      default:
        throw new Error(`unsupported request param tag: ${tag}`)
    }
  }
  return {method, params}
}

const parseInput = async () => {
  const raw = await readStdin()
  return raw.trim() === '' ? {} : JSON.parse(raw)
}

const buildStorage = (storagePairs = []) => {
  const storage = new Map()
  storagePairs.forEach((entry) => {
    const key = Buffer.from(entry.key_b64, 'base64').toString('utf8')
    const value = Uint8Array.from(Buffer.from(entry.value_b64, 'base64'))
    storage.set(key, value)
  })
  return storage
}

const storagePairsOf = (storage) =>
  Array.from(storage.entries())
    .sort(([a], [b]) => a.localeCompare(b))
    .map(([key, value]) => ({
      key_b64: Buffer.from(key, 'utf8').toString('base64'),
      value_b64: Buffer.from(value).toString('base64')
    }))

const manifestFromExports = (exports) => {
  const names = new Set(exports.filter((entry) => entry.kind === 'function').map((entry) => entry.name))
  const methods = []
  if (names.has('octra_query')) {
    methods.push({name: 'octra_query', view: true})
  }
  if (names.has('octra_update')) {
    methods.push({name: 'octra_update', view: false})
  }
  if (names.has('octra_http_request')) {
    methods.push({name: 'octra_http_request', view: true})
  }
  return {methods}
}

const validateImports = (moduleImports) => {
  for (const entry of moduleImports) {
    if (entry.module !== 'octra') {
      throw new Error(`unsupported wasm import module: ${entry.module}`)
    }
    if (entry.kind !== 'function') {
      throw new Error(`unsupported wasm import kind: ${entry.kind}`)
    }
    if (!allowedImports.has(entry.name)) {
      throw new Error(`unsupported wasm import: ${entry.name}`)
    }
  }
}

const instantiate = async (wasmBytes, payload) => {
  const module = await WebAssembly.compile(wasmBytes)
  const moduleImports = WebAssembly.Module.imports(module)
  const moduleExports = WebAssembly.Module.exports(module)
  validateImports(moduleImports)

  const exportsByName = new Set(moduleExports.map((entry) => entry.name))
  if (!exportsByName.has('memory')) {
    throw new Error('wasm_v1 module must export memory')
  }
  if (!exportsByName.has('octra_alloc')) {
    throw new Error('wasm_v1 module must export octra_alloc')
  }
  if (
    !exportsByName.has('octra_query')
    && !exportsByName.has('octra_update')
    && !exportsByName.has('octra_http_request')
  ) {
    throw new Error('wasm_v1 module must export octra_query, octra_update, or octra_http_request')
  }

  const storage = buildStorage(payload.storage_pairs || [])
  const hfheCaps = new Set(payload.hfhe_caps || [])
  const hfhePubkeys = new Map(
    Array.isArray(payload.hfhe_pubkeys)
      ? payload.hfhe_pubkeys
          .filter((entry) => entry && typeof entry.addr === 'string' && typeof entry.pubkey_b64 === 'string')
          .map((entry) => [entry.addr, entry.pubkey_b64])
      : []
  )
  const hfheActiveKey =
    payload.hfhe_active_key
    && typeof payload.hfhe_active_key.key_id === 'string'
    && typeof payload.hfhe_active_key.pubkey_b64 === 'string'
    && typeof payload.hfhe_active_key.seckey_b64 === 'string'
      ? {
          key_id: payload.hfhe_active_key.key_id,
          pubkey_b64: payload.hfhe_active_key.pubkey_b64,
          seckey_b64: payload.hfhe_active_key.seckey_b64,
        }
      : null
  const callerBytes = utf8.encode(payload.caller || '')
  const selfBytes = utf8.encode(payload.address || '')
  let instance = null
  let responseChunks = []
  let responseStatus = 0
  const events = []
  const spawns = []
  const assets = []
  const encryptedAssets = []
  let circleInvokeCache = null
  let circleInvokeToken = null
  let hfheInvokeCache = null
  let hfheInvokeToken = null

  const memoryView = () => {
    if (!instance) {
      throw new Error('wasm instance not ready')
    }
    return new Uint8Array(instance.exports.memory.buffer)
  }

  const readGuestBytes = (ptr, len) => {
    if (ptr < 0 || len < 0) {
      throw new Error('negative pointer')
    }
    const mem = memoryView()
    if (ptr + len > mem.length) {
      throw new Error('guest memory read out of bounds')
    }
    return mem.slice(ptr, ptr + len)
  }

  const readGuestUtf8 = (ptr, len) => utf8Decoder.decode(readGuestBytes(ptr, len))

  const writeGuestBytes = (ptr, bytes) => {
    const mem = memoryView()
    if (ptr < 0 || ptr + bytes.length > mem.length) {
      return -2
    }
    mem.set(bytes, ptr)
    return bytes.length
  }

  const imports = {
    octra: {
      host_response_reset() {
        responseChunks = []
        responseStatus = 0
        return 0
      },
      host_response_write(ptr, len) {
        if (len < 0) {
          return -1
        }
        const chunk = readGuestBytes(ptr, len)
        const total = responseChunks.reduce((sum, part) => sum + part.length, 0) + chunk.length
        if (total > maxResponseBytes) {
          throw new Error('wasm response exceeds limit')
        }
        responseChunks.push(chunk)
        return len
      },
      host_response_finish(statusCode) {
        responseStatus = statusCode | 0
        return 0
      },
      host_circle_invoke_len(reqPtr, reqLen) {
        const requestBytes = readGuestBytes(reqPtr, reqLen)
        const responseBytes = executeCircleInvoke(storage, payload, spawns, assets, encryptedAssets, requestBytes)
        circleInvokeCache = responseBytes
        circleInvokeToken = b64OfBytes(requestBytes)
        return responseBytes.length
      },
      host_circle_invoke(reqPtr, reqLen, outPtr, outCap) {
        const requestBytes = readGuestBytes(reqPtr, reqLen)
        const requestToken = b64OfBytes(requestBytes)
        const responseBytes =
          circleInvokeCache && circleInvokeToken === requestToken
            ? circleInvokeCache
            : executeCircleInvoke(storage, payload, spawns, assets, encryptedAssets, requestBytes)
        if (outCap < responseBytes.length) {
          return -2
        }
        const written = writeGuestBytes(outPtr, responseBytes)
        circleInvokeCache = null
        circleInvokeToken = null
        return written
      },
      host_hfhe_invoke_len(reqPtr, reqLen) {
        const requestBytes = readGuestBytes(reqPtr, reqLen)
        const responseBytes = executeHfheInvoke(hfheCaps, hfhePubkeys, hfheActiveKey, requestBytes)
        hfheInvokeCache = responseBytes
        hfheInvokeToken = b64OfBytes(requestBytes)
        return responseBytes.length
      },
      host_hfhe_invoke(reqPtr, reqLen, outPtr, outCap) {
        const requestBytes = readGuestBytes(reqPtr, reqLen)
        const requestToken = b64OfBytes(requestBytes)
        const responseBytes =
          hfheInvokeCache && hfheInvokeToken === requestToken
            ? hfheInvokeCache
            : executeHfheInvoke(hfheCaps, hfhePubkeys, hfheActiveKey, requestBytes)
        if (outCap < responseBytes.length) {
          return -2
        }
        const written = writeGuestBytes(outPtr, responseBytes)
        hfheInvokeCache = null
        hfheInvokeToken = null
        return written
      },
      host_kv_get_len(keyPtr, keyLen) {
        const key = readGuestUtf8(keyPtr, keyLen)
        const value = storage.get(key)
        return value ? value.length : -1
      },
      host_kv_get(keyPtr, keyLen, outPtr, outCap) {
        const key = readGuestUtf8(keyPtr, keyLen)
        const value = storage.get(key)
        if (!value) {
          return -1
        }
        if (outCap < value.length) {
          return -2
        }
        return writeGuestBytes(outPtr, value)
      },
      host_kv_put(keyPtr, keyLen, valuePtr, valueLen) {
        if (payload.is_view) {
          return -1
        }
        const key = readGuestUtf8(keyPtr, keyLen)
        const value = readGuestBytes(valuePtr, valueLen)
        storage.set(key, value)
        return 0
      },
      host_kv_del(keyPtr, keyLen) {
        if (payload.is_view) {
          return -1
        }
        const key = readGuestUtf8(keyPtr, keyLen)
        storage.delete(key)
        return 0
      },
      host_epoch() {
        return BigInt(payload.current_epoch || 0)
      },
      host_caller_len() {
        return callerBytes.length
      },
      host_caller_read(outPtr, outCap) {
        if (outCap < callerBytes.length) {
          return -2
        }
        return writeGuestBytes(outPtr, callerBytes)
      },
      host_self_len() {
        return selfBytes.length
      },
      host_self_read(outPtr, outCap) {
        if (outCap < selfBytes.length) {
          return -2
        }
        return writeGuestBytes(outPtr, selfBytes)
      },
      host_state_path_key_len(stateRefPtr, stateRefLen) {
        const encoded = utf8.encode(statePathKey(readGuestUtf8(stateRefPtr, stateRefLen)))
        return encoded.length
      },
      host_state_path_key(stateRefPtr, stateRefLen, outPtr, outCap) {
        const encoded = utf8.encode(statePathKey(readGuestUtf8(stateRefPtr, stateRefLen)))
        if (outCap < encoded.length) {
          return -2
        }
        return writeGuestBytes(outPtr, encoded)
      },
      host_emit_event(topicPtr, topicLen, dataPtr, dataLen) {
        if (payload.is_view) {
          return -1
        }
        const topic = readGuestBytes(topicPtr, topicLen)
        const data = readGuestBytes(dataPtr, dataLen)
        events.push({
          topic_b64: b64OfBytes(topic),
          data_b64: b64OfBytes(data)
        })
        return 0
      }
    }
  }

  instance = await WebAssembly.instantiate(module, imports)
  const memory = instance.exports.memory
  if (!(memory instanceof WebAssembly.Memory)) {
    throw new Error('wasm_v1 memory export missing')
  }
  const initialPages = Math.floor(memory.buffer.byteLength / pageBytes)
  if (initialPages > maxInitialPages) {
    throw new Error(`wasm_v1 memory exceeds ${maxInitialPages} pages`)
  }

  return {
    instance,
    storage,
    exports: moduleExports,
    events,
    spawns,
    assets,
    encryptedAssets,
    getResponseBytes: () => {
      const total = responseChunks.reduce((sum, part) => sum + part.length, 0)
      const out = new Uint8Array(total)
      let offset = 0
      responseChunks.forEach((part) => {
        out.set(part, offset)
        offset += part.length
      })
      return out
    },
    getResponseStatus: () => responseStatus
  }
}

const callExport = ({instance, getResponseBytes}, exportName, requestBytes) => {
  const fn = instance.exports[exportName]
  if (typeof fn !== 'function') {
    throw new Error(`wasm export not found: ${exportName}`)
  }
  const alloc = instance.exports.octra_alloc
  if (typeof alloc !== 'function') {
    throw new Error('wasm export not found: octra_alloc')
  }
  const ptr = Number(alloc(requestBytes.length))
  if (!Number.isInteger(ptr) || ptr < 0) {
    throw new Error('invalid octra_alloc pointer')
  }
  const memory = new Uint8Array(instance.exports.memory.buffer)
  if (ptr + requestBytes.length > memory.length) {
    throw new Error('octra_alloc returned out-of-bounds pointer')
  }
  memory.set(requestBytes, ptr)
  const code = Number(fn(ptr, requestBytes.length))
  return {
    code,
    responseBytes: getResponseBytes()
  }
}

const runDescribe = async (payload) => {
  const wasmBytes = bytesOfB64(payload.code_b64)
  const runtime = await instantiate(wasmBytes, {
    ...payload,
    storage_pairs: [],
    caller: '',
    address: '',
    current_epoch: 0,
    is_view: true
  })
  let manifest = null
  if (typeof runtime.instance.exports.octra_manifest === 'function') {
    try {
      const {responseBytes} = callExport(runtime, 'octra_manifest', utf8.encode(''))
      manifest = parseManifestResponse(responseBytes)
    } catch (_error) {
      manifest = null
    }
  }
  writeJson({
    exports: runtime.exports.map((entry) => ({name: entry.name, kind: entry.kind})),
    manifest
  })
}

const runValidate = async (payload) => {
  const wasmBytes = bytesOfB64(payload.code_b64)
  const runtime = await instantiate(wasmBytes, {
    ...payload,
    storage_pairs: [],
    caller: '',
    address: '',
    current_epoch: 0,
    is_view: true
  })
  writeJson({
    exports: runtime.exports.map((entry) => ({name: entry.name, kind: entry.kind})),
    manifest: manifestFromExports(runtime.exports)
  })
}

const runExecute = async (payload) => {
  const wasmBytes = bytesOfB64(payload.code_b64)
  const requestBytes = bytesOfB64(payload.request_b64)
  const runtime = await instantiate(wasmBytes, payload)
  const {code, responseBytes} = callExport(runtime, payload.export_name, requestBytes)
  writeJson({
    success: code === 0,
    status_code: runtime.getResponseStatus(),
    response_b64: responseBytes.length === 0 ? null : b64OfBytes(responseBytes),
    storage_pairs: storagePairsOf(runtime.storage),
    events: runtime.events,
    spawns: runtime.spawns,
    assets: runtime.assets,
    encrypted_assets: runtime.encryptedAssets,
    error: code === 0 ? null : `wasm export returned ${code}`
  })
}

const main = async () => {
  try {
    const payload = await parseInput()
    switch (payload.action) {
      case 'validate':
        await runValidate(payload)
        break
      case 'describe':
        await runDescribe(payload)
        break
      case 'execute':
        await runExecute(payload)
        break
      default:
        throw new Error(`unsupported action: ${payload.action || '<none>'}`)
    }
  } catch (error) {
    fail(error instanceof Error ? error.message : String(error))
  }
}

await main()