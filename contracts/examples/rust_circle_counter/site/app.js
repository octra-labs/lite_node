const result = document.getElementById('result')

const demoState = {
  room: null,
  registerLane: null
}

const show = (label, value) => {
  result.textContent = JSON.stringify({ label, value }, null, 2)
}

const walletContext = async () => {
  const [walletInfo, walletBalance] = await Promise.allSettled([
    window.OctraCircle.request('wallet.info'),
    window.OctraCircle.request('wallet.balance')
  ])
  return {
    wallet: walletInfo.status === 'fulfilled' ? walletInfo.value : { error: walletInfo.reason?.message || String(walletInfo.reason) },
    balance: walletBalance.status === 'fulfilled' ? walletBalance.value : { error: walletBalance.reason?.message || String(walletBalance.reason) }
  }
}

const settledValueOf = (settled) =>
  settled.status === 'fulfilled'
    ? settled.value
    : { error: settled.reason?.message || String(settled.reason) }

const programInfoFallback = async (message) => {
  const [context, runtimeGuard, counter, caller] = await Promise.allSettled([
    walletContext(),
    window.OctraCircle.request('program.view', { method: 'runtime_guard', params: [] }),
    window.OctraCircle.request('program.view', { method: 'get_counter', params: [] }),
    window.OctraCircle.request('program.view', { method: 'caller_echo', params: [] })
  ])
  return {
    owner_auth_required: true,
    error: message,
    hint: 'sealed runtime descriptors can be owner-gated; showing live runtime summary instead',
    context: settledValueOf(context),
    runtime_guard: settledValueOf(runtimeGuard),
    get_counter: settledValueOf(counter),
    caller_echo: settledValueOf(caller)
  }
}

const diagnosticErrorOf = async (label, error) => {
  const message = error?.message || String(error)
  if (message.includes('circle owner authorization required')) {
    return {
      label,
      error: message,
      hint: 'program.info can be owner-auth on sealed runtime circles; use Wallet info, Runtime guard, Get counter, and the write actions to exercise the live runtime path'
    }
  }
  if (!message.includes('sender not found')) {
    return { label, error: message }
  }
  return {
    label,
    error: message,
    hint: 'active wallet sender is missing on this RPC; confirm the browser is attached to the funded local wallet on http://127.0.0.1:18421 and RPC http://127.0.0.1:18081',
    context: await walletContext()
  }
}

const run = async (label, action) => {
  try {
    const value = await action()
    show(label, value)
  } catch (error) {
    result.textContent = JSON.stringify(await diagnosticErrorOf(label, error), null, 2)
  }
}

const hexOf = bytes => Array.from(bytes).map((byte) => byte.toString(16).padStart(2, '0')).join('')

const base64Of = bytes => {
  let text = ''
  bytes.forEach((byte) => {
    text += String.fromCharCode(byte)
  })
  return btoa(text)
}

const sha256HexOf = async (text) => {
  const bytes = new TextEncoder().encode(text)
  const digest = await crypto.subtle.digest('SHA-256', bytes)
  return hexOf(new Uint8Array(digest))
}

const sha256BytesOf = async (text) => {
  const bytes = new TextEncoder().encode(text)
  const digest = await crypto.subtle.digest('SHA-256', bytes)
  return new Uint8Array(digest)
}

const uniqueHex = (label) => sha256HexOf(`${label}|${Date.now()}|${Math.random()}`)

const uniqueB64 = async (label) => base64Of(await sha256BytesOf(`${label}|${Date.now()}|${Math.random()}`))

const buildRoomState = async (subjectAddr) => {
  const stateRef = await uniqueHex('balance-state')
  const objectRef = await uniqueHex('object-ref')
  const memberRef = await uniqueHex('balance-member')
  const workflowRef = await uniqueHex('balance-workflow')
  const transitionRef = await uniqueHex('transition-ref')
  const ciphertextCommitment = await uniqueB64('ciphertext-commitment')
  const amountCommitment = await uniqueB64('amount-commitment')
  const proofReceiptHash = await uniqueHex('proof-receipt')
  return {
    stateRef,
    objectRef,
    memberRef,
    subjectAddr,
    workflowRef,
    transitionRef,
    ciphertextCommitment,
    amountCommitment,
    proofReceiptHash
  }
}

const buildRegisterState = async (previous) => {
  const registerStateRef = await uniqueHex('register-state')
  const registerRef = await uniqueHex('register-ref')
  const registerWorkflowRef = await uniqueHex('register-workflow')
  const registerCiphertextCommitment = await uniqueB64('register-ciphertext-commitment')
  const registerProofReceiptHash = await uniqueHex('register-proof-receipt')
  const nextStateRef = await uniqueHex('next-object-state')
  const registerMemberRef = await uniqueHex('register-member')
  return {
    registerStateRef,
    registerRef,
    registerWorkflowRef,
    registerCiphertextCommitment,
    registerProofReceiptHash,
    nextStateRef,
    registerMemberRef,
    previous
  }
}

document.getElementById('wallet-info').addEventListener('click', () => run('wallet.info', walletContext))

document.getElementById('program-info').addEventListener('click', () => run('program.info', async () => {
  try {
    return await window.OctraCircle.request('program.info')
  } catch (error) {
    const message = error?.message || String(error)
    if (!message.includes('circle owner authorization required')) {
      throw error
    }
    return programInfoFallback(message)
  }
}))

document.getElementById('runtime-guard').addEventListener('click', () => run('runtime_guard', () =>
  window.OctraCircle.request('program.view', { method: 'runtime_guard', params: [] })
))

document.getElementById('get-counter').addEventListener('click', () => run('get_counter', () =>
  window.OctraCircle.request('program.view', { method: 'get_counter', params: [] })
))

document.getElementById('inc').addEventListener('click', () => run('inc', async () => {
  const context = await walletContext()
  const tx = await window.OctraCircle.request('program.call', {
    method: 'inc',
    params: [],
    amount: '0',
    ou: '1000'
  })
  const counter = await window.OctraCircle.request('program.view', { method: 'get_counter', params: [] })
  return { tx, counter, context }
}))

document.getElementById('prepare-room').addEventListener('click', () => run('prepare_room', async () => {
  const context = await walletContext()
  const subjectAddr = context.wallet?.address
  if (!subjectAddr) {
    throw new Error('wallet.info did not return an active address')
  }
  demoState.room = await buildRoomState(subjectAddr)
  const params = [
    demoState.room.stateRef,
    demoState.room.objectRef,
    demoState.room.memberRef,
    demoState.room.subjectAddr,
    demoState.room.workflowRef,
    demoState.room.transitionRef,
    demoState.room.ciphertextCommitment,
    demoState.room.amountCommitment,
    demoState.room.proofReceiptHash
  ]
  const tx = await window.OctraCircle.request('program.call', {
    method: 'prepare_room',
    params,
    amount: '0',
    ou: '1000'
  })
  const stateClass = await window.OctraCircle.request('program.view', {
    method: 'state_class_of',
    params: [demoState.room.stateRef]
  })
  const objectStatus = await window.OctraCircle.request('program.view', {
    method: 'object_status_of',
    params: [demoState.room.objectRef]
  })
  const workflowStatus = await window.OctraCircle.request('program.view', {
    method: 'balance_workflow_status_of',
    params: [demoState.room.workflowRef]
  })
  return { tx, room: demoState.room, stateClass, objectStatus, workflowStatus, context }
}))

document.getElementById('prepare-register').addEventListener('click', () => run('prepare_register_lane', async () => {
  if (!demoState.room) {
    throw new Error('prepare_room must run first')
  }
  demoState.registerLane = await buildRegisterState(demoState.room)
  const params = [
    demoState.registerLane.registerStateRef,
    demoState.registerLane.registerRef,
    demoState.registerLane.registerWorkflowRef,
    demoState.registerLane.registerCiphertextCommitment,
    demoState.registerLane.registerProofReceiptHash,
    demoState.registerLane.nextStateRef
  ]
  const tx = await window.OctraCircle.request('program.call', {
    method: 'prepare_register_lane',
    params,
    amount: '0',
    ou: '1000'
  })
  const workflowStatus = await window.OctraCircle.request('program.view', {
    method: 'register_workflow_status_of',
    params: [demoState.registerLane.registerWorkflowRef]
  })
  return { tx, registerLane: demoState.registerLane, workflowStatus, subjectAddr: demoState.room.subjectAddr }
}))

document.getElementById('promote-room').addEventListener('click', () => run('promote_room', async () => {
  if (!demoState.room || !demoState.registerLane) {
    throw new Error('prepare_room and prepare_register_lane must run first')
  }
  const params = [
    demoState.room.transitionRef,
    demoState.room.objectRef,
    demoState.room.stateRef,
    demoState.registerLane.nextStateRef,
    demoState.room.memberRef,
    demoState.room.stateRef,
    demoState.registerLane.registerMemberRef,
    demoState.registerLane.registerStateRef,
    demoState.room.proofReceiptHash,
    demoState.room.transitionRef
  ]
  const tx = await window.OctraCircle.request('program.call', {
    method: 'promote_room',
    params,
    amount: '0',
    ou: '1000'
  })
  const objectStatus = await window.OctraCircle.request('program.view', {
    method: 'object_status_of',
    params: [demoState.room.objectRef]
  })
  const lastTransition = await window.OctraCircle.request('program.view', {
    method: 'object_last_transition_of',
    params: [demoState.room.objectRef]
  })
  return { tx, objectStatus, lastTransition, room: demoState.room, registerLane: demoState.registerLane }
}))