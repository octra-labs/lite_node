// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

use std::string::String;
use std::vec::Vec;

const REQUEST_MAGIC: &[u8; 5] = b"OCWR1";
const RESPONSE_MAGIC: &[u8; 5] = b"OCWS1";

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value {
    Null,
    Bool(bool),
    Int(String),
    String(String),
}

impl Value {
    pub fn as_string(&self) -> Option<&str> {
        match self {
            Self::String(value) | Self::Int(value) => Some(value.as_str()),
            Self::Null | Self::Bool(_) => None,
        }
    }

    pub fn as_i64(&self) -> Option<i64> {
        match self {
            Self::Int(value) => value.parse::<i64>().ok(),
            Self::Null | Self::Bool(_) | Self::String(_) => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Self::Bool(value) => Some(*value),
            Self::Null | Self::Int(_) | Self::String(_) => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Request {
    pub method: String,
    pub params: Vec<Value>,
}

impl Request {
    pub fn decode(raw: &[u8]) -> Result<Self, i32> {
        if raw.len() < REQUEST_MAGIC.len() + 4 {
            return Err(10);
        }
        if &raw[..REQUEST_MAGIC.len()] != REQUEST_MAGIC {
            return Err(11);
        }
        let mut offset = REQUEST_MAGIC.len();
        let method_len = decode_u16(raw, &mut offset)? as usize;
        let method = decode_string(raw, &mut offset, method_len)?;
        let param_count = decode_u16(raw, &mut offset)? as usize;
        let mut params = Vec::with_capacity(param_count);
        for _ in 0..param_count {
            let tag = decode_u8(raw, &mut offset)?;
            let size = decode_u32(raw, &mut offset)? as usize;
            let payload = decode_bytes(raw, &mut offset, size)?;
            let value = match tag {
                0 => Value::Null,
                1 => Value::Bool(false),
                2 => Value::Bool(true),
                3 => Value::Int(String::from_utf8(payload.to_vec()).map_err(|_| 12)?),
                4 => Value::String(String::from_utf8(payload.to_vec()).map_err(|_| 12)?),
                _ => return Err(13),
            };
            params.push(value);
        }
        if offset != raw.len() {
            return Err(14);
        }
        Ok(Self { method, params })
    }

    pub fn param(&self, idx: usize) -> Option<&Value> {
        self.params.get(idx)
    }

    pub fn string_param(&self, idx: usize) -> Result<&str, i32> {
        self.param(idx).and_then(Value::as_string).ok_or(20)
    }

    pub fn int_param(&self, idx: usize) -> Result<i64, i32> {
        self.param(idx).and_then(Value::as_i64).ok_or(21)
    }

    pub fn bool_param(&self, idx: usize) -> Result<bool, i32> {
        self.param(idx).and_then(Value::as_bool).ok_or(22)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StateClass {
    OpaqueCell,
    CipherCell,
    BalanceCell,
    RegisterCell,
    ProgramObject,
}

impl StateClass {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::OpaqueCell => "opaque_cell",
            Self::CipherCell => "cipher_cell",
            Self::BalanceCell => "balance_cell",
            Self::RegisterCell => "register_cell",
            Self::ProgramObject => "program_object",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HfheProfile {
    None,
    CiphertextV1,
    BalanceCellV1,
    RegisterCellV1,
    CommitBoundV1,
}

impl HfheProfile {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::None => "none",
            Self::CiphertextV1 => "ciphertext_v1",
            Self::BalanceCellV1 => "balance_cell_v1",
            Self::RegisterCellV1 => "register_cell_v1",
            Self::CommitBoundV1 => "commit_bound_v1",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ProofKind {
    None,
    ZeroReceiptV1,
    RangeV1,
    RangeReceiptV1,
    BoundZeroV1,
    BoundZeroReceiptV1,
}

impl ProofKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::None => "none",
            Self::ZeroReceiptV1 => "zero_receipt_v1",
            Self::RangeV1 => "range_v1",
            Self::RangeReceiptV1 => "range_receipt_v1",
            Self::BoundZeroV1 => "bound_zero_v1",
            Self::BoundZeroReceiptV1 => "bound_zero_receipt_v1",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TransitionMode {
    Open,
    ProofRequired,
    Frozen,
}

impl TransitionMode {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Open => "open",
            Self::ProofRequired => "proof_required",
            Self::Frozen => "frozen",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StateDescriptorField {
    StateClass,
    Codec,
    SchemaHash,
    SubjectAddr,
    HfheProfile,
    MutableState,
}

impl StateDescriptorField {
    fn as_str(&self) -> &'static str {
        match self {
            Self::StateClass => "state_class",
            Self::Codec => "codec",
            Self::SchemaHash => "schema_hash",
            Self::SubjectAddr => "subject_addr",
            Self::HfheProfile => "hfhe_profile",
            Self::MutableState => "mutable_state",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StatePolicyField {
    DeliveryKeyId,
    ActivateAfterEpoch,
    ExpireAfterEpoch,
    Tombstone,
    Revoked,
}

impl StatePolicyField {
    fn as_str(&self) -> &'static str {
        match self {
            Self::DeliveryKeyId => "delivery_key_id",
            Self::ActivateAfterEpoch => "activate_after_epoch",
            Self::ExpireAfterEpoch => "expire_after_epoch",
            Self::Tombstone => "tombstone",
            Self::Revoked => "revoked",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BalanceCellField {
    CiphertextCommitment,
    AmountCommitment,
    ProofKind,
    ProofReceiptHash,
}

impl BalanceCellField {
    fn as_str(&self) -> &'static str {
        match self {
            Self::CiphertextCommitment => "ciphertext_commitment",
            Self::AmountCommitment => "amount_commitment",
            Self::ProofKind => "proof_kind",
            Self::ProofReceiptHash => "proof_receipt_hash",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RegisterCellField {
    CiphertextCommitment,
    ProofKind,
    ProofReceiptHash,
}

impl RegisterCellField {
    fn as_str(&self) -> &'static str {
        match self {
            Self::CiphertextCommitment => "ciphertext_commitment",
            Self::ProofKind => "proof_kind",
            Self::ProofReceiptHash => "proof_receipt_hash",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BalanceBindingField {
    CurrentStateRef,
    Version,
    Status,
    LastWorkflowRef,
}

impl BalanceBindingField {
    fn as_str(&self) -> &'static str {
        match self {
            Self::CurrentStateRef => "current_state_ref",
            Self::Version => "version",
            Self::Status => "status",
            Self::LastWorkflowRef => "last_workflow_ref",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RegisterBindingField {
    CurrentStateRef,
    Version,
    Status,
    LastWorkflowRef,
}

impl RegisterBindingField {
    fn as_str(&self) -> &'static str {
        match self {
            Self::CurrentStateRef => "current_state_ref",
            Self::Version => "version",
            Self::Status => "status",
            Self::LastWorkflowRef => "last_workflow_ref",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BalanceWorkflowField {
    FlowKind,
    DebitSubjectAddr,
    CreditSubjectAddr,
    DebitStateRef,
    CreditStateRef,
    AmountCommitment,
    ProofKind,
    ProofReceiptHash,
    Status,
    IntentId,
}

impl BalanceWorkflowField {
    fn as_str(&self) -> &'static str {
        match self {
            Self::FlowKind => "flow_kind",
            Self::DebitSubjectAddr => "debit_subject_addr",
            Self::CreditSubjectAddr => "credit_subject_addr",
            Self::DebitStateRef => "debit_state_ref",
            Self::CreditStateRef => "credit_state_ref",
            Self::AmountCommitment => "amount_commitment",
            Self::ProofKind => "proof_kind",
            Self::ProofReceiptHash => "proof_receipt_hash",
            Self::Status => "status",
            Self::IntentId => "intent_id",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RegisterWorkflowField {
    RegisterRef,
    PreviousStateRef,
    NextStateRef,
    WorkflowKind,
    ProofKind,
    ProofReceiptHash,
    Status,
    IntentId,
}

impl RegisterWorkflowField {
    fn as_str(&self) -> &'static str {
        match self {
            Self::RegisterRef => "register_ref",
            Self::PreviousStateRef => "previous_state_ref",
            Self::NextStateRef => "next_state_ref",
            Self::WorkflowKind => "workflow_kind",
            Self::ProofKind => "proof_kind",
            Self::ProofReceiptHash => "proof_receipt_hash",
            Self::Status => "status",
            Self::IntentId => "intent_id",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ObjectBindingField {
    CurrentStateRef,
    Version,
    Status,
    LastTransitionRef,
}

impl ObjectBindingField {
    fn as_str(&self) -> &'static str {
        match self {
            Self::CurrentStateRef => "current_state_ref",
            Self::Version => "version",
            Self::Status => "status",
            Self::LastTransitionRef => "last_transition_ref",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ObjectMemberField {
    StateRef,
    MemberKind,
    StateClass,
    Codec,
    Status,
}

impl ObjectMemberField {
    fn as_str(&self) -> &'static str {
        match self {
            Self::StateRef => "state_ref",
            Self::MemberKind => "member_kind",
            Self::StateClass => "state_class",
            Self::Codec => "codec",
            Self::Status => "status",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ObjectPolicyField {
    DeliveryKeyId,
    ActivateAfterEpoch,
    ExpireAfterEpoch,
    Tombstone,
    Revoked,
    TransitionMode,
    RequiredProofKind,
    MemberQuorum,
    AllowDetach,
    AllowRootStateRotation,
}

impl ObjectPolicyField {
    fn as_str(&self) -> &'static str {
        match self {
            Self::DeliveryKeyId => "delivery_key_id",
            Self::ActivateAfterEpoch => "activate_after_epoch",
            Self::ExpireAfterEpoch => "expire_after_epoch",
            Self::Tombstone => "tombstone",
            Self::Revoked => "revoked",
            Self::TransitionMode => "transition_mode",
            Self::RequiredProofKind => "required_proof_kind",
            Self::MemberQuorum => "member_quorum",
            Self::AllowDetach => "allow_detach",
            Self::AllowRootStateRotation => "allow_root_state_rotation",
        }
    }
}

pub struct StateDescriptorInput<'a> {
    pub state_class: StateClass,
    pub codec: &'a str,
    pub schema_hash: Option<&'a str>,
    pub subject_addr: Option<&'a str>,
    pub hfhe_profile: HfheProfile,
    pub mutable_state: bool,
}

pub struct StatePolicyInput<'a> {
    pub delivery_key_id: Option<&'a str>,
    pub activate_after_epoch: i64,
    pub expire_after_epoch: i64,
}

pub struct BalanceCellInput<'a> {
    pub ciphertext_commitment: &'a str,
    pub amount_commitment: &'a str,
    pub proof_kind: ProofKind,
    pub proof_receipt_hash: Option<&'a str>,
}

pub struct RegisterCellInput<'a> {
    pub ciphertext_commitment: &'a str,
    pub proof_kind: ProofKind,
    pub proof_receipt_hash: Option<&'a str>,
}

pub struct BalanceWorkflowInput<'a> {
    pub flow_kind: &'a str,
    pub debit_subject_addr: &'a str,
    pub credit_subject_addr: &'a str,
    pub debit_state_ref: &'a str,
    pub credit_state_ref: &'a str,
    pub amount_commitment: &'a str,
    pub proof_kind: ProofKind,
    pub proof_receipt_hash: &'a str,
    pub status: &'a str,
    pub intent_id: &'a str,
}

pub struct RegisterWorkflowInput<'a> {
    pub register_ref: &'a str,
    pub previous_state_ref: &'a str,
    pub next_state_ref: &'a str,
    pub workflow_kind: &'a str,
    pub proof_kind: ProofKind,
    pub proof_receipt_hash: &'a str,
    pub status: &'a str,
    pub intent_id: &'a str,
}

pub struct ObjectMemberInput<'a> {
    pub state_ref: &'a str,
    pub member_kind: &'a str,
    pub state_class: StateClass,
    pub codec: &'a str,
    pub status: &'a str,
}

pub struct ObjectPolicyInput<'a> {
    pub delivery_key_id: Option<&'a str>,
    pub activate_after_epoch: i64,
    pub expire_after_epoch: i64,
    pub transition_mode: TransitionMode,
    pub required_proof_kind: ProofKind,
    pub member_quorum: i64,
    pub allow_detach: bool,
    pub allow_root_state_rotation: bool,
}

pub struct ObjectTransitionInput<'a> {
    pub transition_ref: &'a str,
    pub object_ref: &'a str,
    pub previous_state_ref: &'a str,
    pub next_state_ref: &'a str,
    pub proof_kind: ProofKind,
    pub proof_receipt_hash: &'a str,
    pub status: &'a str,
    pub intent_id: &'a str,
}

pub enum ObjectMemberDelta<'a> {
    Attach {
        member_ref: &'a str,
        input: ObjectMemberInput<'a>,
    },
    Detach {
        member_ref: &'a str,
    },
}

pub struct BalanceRoomInput<'a> {
    pub state_ref: &'a str,
    pub descriptor: StateDescriptorInput<'a>,
    pub policy: StatePolicyInput<'a>,
    pub cell: BalanceCellInput<'a>,
    pub subject_addr: &'a str,
    pub workflow_ref: &'a str,
    pub binding_status: &'a str,
    pub workflow: BalanceWorkflowInput<'a>,
    pub object_ref: &'a str,
    pub object_transition_ref: &'a str,
    pub object_status: &'a str,
    pub object_policy: ObjectPolicyInput<'a>,
    pub object_member_ref: &'a str,
    pub object_member: ObjectMemberInput<'a>,
}

pub struct RegisterLaneInput<'a> {
    pub state_ref: &'a str,
    pub descriptor: StateDescriptorInput<'a>,
    pub cell: RegisterCellInput<'a>,
    pub register_ref: &'a str,
    pub workflow_ref: &'a str,
    pub binding_status: &'a str,
    pub workflow: RegisterWorkflowInput<'a>,
}

pub struct Host;

impl Host {
    pub fn caller() -> Result<String, i32> {
        read_len_prefixed(host_caller_len, host_caller_read)
    }

    pub fn self_addr() -> Result<String, i32> {
        read_len_prefixed(host_self_len, host_self_read)
    }

    pub fn epoch() -> u64 {
        unsafe { host_epoch() as u64 }
    }

    pub fn circle_spawn(payload_json: &str, owner_mode: &str) -> Result<String, i32> {
        expect_string(Self::circle_invoke(
            "circle_spawn",
            &[
                Value::String(payload_json.to_owned()),
                Value::String(owner_mode.to_owned()),
            ],
        )?)
    }

    pub fn circle_public_asset_meta(
        circle_id: &str,
        canonical_path: &str,
        offset: usize,
        max_bytes: usize,
    ) -> Result<String, i32> {
        expect_string(Self::circle_invoke(
            "circle_public_asset_meta",
            &[
                Value::String(circle_id.to_owned()),
                Value::String(canonical_path.to_owned()),
                Value::Int(offset.to_string()),
                Value::Int(max_bytes.to_string()),
            ],
        )?)
    }

    pub fn circle_public_asset_read(
        circle_id: &str,
        canonical_path: &str,
        offset: usize,
        max_bytes: usize,
    ) -> Result<String, i32> {
        expect_string(Self::circle_invoke(
            "circle_public_asset_read",
            &[
                Value::String(circle_id.to_owned()),
                Value::String(canonical_path.to_owned()),
                Value::Int(offset.to_string()),
                Value::Int(max_bytes.to_string()),
            ],
        )?)
    }

    pub fn circle_asset_put(
        circle_id: &str,
        path: &str,
        content_type: &str,
        body_b64: &str,
        encoding: Option<&str>,
    ) -> Result<bool, i32> {
        expect_bool(Self::circle_invoke(
            "circle_asset_put",
            &[
                Value::String(circle_id.to_owned()),
                Value::String(path.to_owned()),
                Value::String(content_type.to_owned()),
                Value::String(body_b64.to_owned()),
                match encoding {
                    Some(value) => Value::String(value.to_owned()),
                    None => Value::Null,
                },
            ],
        )?)
    }

    pub fn circle_asset_put_encrypted(
        circle_id: &str,
        path: &str,
        content_type: &str,
        ciphertext_b64: &str,
        key_id: &str,
        plaintext_hash: &str,
        encoding: Option<&str>,
        padding_class: Option<&str>,
        activate_after_epoch: Option<&str>,
        expire_after_epoch: Option<&str>,
        metadata_mode: Option<&str>,
    ) -> Result<bool, i32> {
        Self::circle_asset_put_encrypted_at(
            circle_id,
            "path",
            path,
            content_type,
            ciphertext_b64,
            key_id,
            plaintext_hash,
            encoding,
            padding_class,
            activate_after_epoch,
            expire_after_epoch,
            metadata_mode,
        )
    }

    pub fn circle_asset_put_encrypted_at(
        circle_id: &str,
        locator_kind: &str,
        locator_value: &str,
        content_type: &str,
        ciphertext_b64: &str,
        key_id: &str,
        plaintext_hash: &str,
        encoding: Option<&str>,
        padding_class: Option<&str>,
        activate_after_epoch: Option<&str>,
        expire_after_epoch: Option<&str>,
        metadata_mode: Option<&str>,
    ) -> Result<bool, i32> {
        expect_bool(Self::circle_invoke(
            "circle_asset_put_encrypted_at",
            &[
                Value::String(circle_id.to_owned()),
                Value::String(locator_kind.to_owned()),
                Value::String(locator_value.to_owned()),
                Value::String(content_type.to_owned()),
                Value::String(ciphertext_b64.to_owned()),
                Value::String(key_id.to_owned()),
                Value::String(plaintext_hash.to_owned()),
                match encoding {
                    Some(value) => Value::String(value.to_owned()),
                    None => Value::Null,
                },
                match padding_class {
                    Some(value) => Value::String(value.to_owned()),
                    None => Value::Null,
                },
                match activate_after_epoch {
                    Some(value) => Value::String(value.to_owned()),
                    None => Value::Null,
                },
                match expire_after_epoch {
                    Some(value) => Value::String(value.to_owned()),
                    None => Value::Null,
                },
                match metadata_mode {
                    Some(value) => Value::String(value.to_owned()),
                    None => Value::Null,
                },
            ],
        )?)
    }

    pub fn state_path_key(state_ref: &str) -> Result<String, i32> {
        let state_bytes = state_ref.as_bytes();
        let out_len = unsafe { host_state_path_key_len(state_bytes.as_ptr(), state_bytes.len() as i32) };
        if out_len < 0 {
            return Err(out_len);
        }
        let mut out = vec![0_u8; out_len as usize];
        let written = unsafe {
            host_state_path_key(
                state_bytes.as_ptr(),
                state_bytes.len() as i32,
                out.as_mut_ptr(),
                out.len() as i32,
            )
        };
        if written < 0 {
            return Err(written);
        }
        out.truncate(written as usize);
        String::from_utf8(out).map_err(|_| 23)
    }

    pub fn kv_get(key: &str) -> Result<Option<Vec<u8>>, i32> {
        let key_bytes = key.as_bytes();
        let len = unsafe { host_kv_get_len(key_bytes.as_ptr(), key_bytes.len() as i32) };
        if len == -1 {
            return Ok(None);
        }
        if len < 0 {
            return Err(len);
        }
        let mut out = vec![0_u8; len as usize];
        let written = unsafe {
            host_kv_get(
                key_bytes.as_ptr(),
                key_bytes.len() as i32,
                out.as_mut_ptr(),
                out.len() as i32,
            )
        };
        if written < 0 {
            return Err(written);
        }
        out.truncate(written as usize);
        Ok(Some(out))
    }

    pub fn kv_get_string(key: &str) -> Result<Option<String>, i32> {
        match Self::kv_get(key)? {
            Some(value) => String::from_utf8(value).map(Some).map_err(|_| 24),
            None => Ok(None),
        }
    }

    pub fn kv_put(key: &str, value: &[u8]) -> Result<(), i32> {
        let code = unsafe {
            host_kv_put(
                key.as_bytes().as_ptr(),
                key.len() as i32,
                value.as_ptr(),
                value.len() as i32,
            )
        };
        if code < 0 {
            return Err(code);
        }
        Ok(())
    }

    pub fn kv_put_string(key: &str, value: &str) -> Result<(), i32> {
        Self::kv_put(key, value.as_bytes())
    }

    pub fn kv_del(key: &str) -> Result<(), i32> {
        let code = unsafe { host_kv_del(key.as_bytes().as_ptr(), key.len() as i32) };
        if code < 0 {
            return Err(code);
        }
        Ok(())
    }

    pub fn session_get(key: &str) -> Result<Option<Vec<u8>>, i32> {
        let key_bytes = key.as_bytes();
        let len = unsafe { host_session_get_len(key_bytes.as_ptr(), key_bytes.len() as i32) };
        if len == -1 {
            return Ok(None);
        }
        if len < 0 {
            return Err(len);
        }
        let mut out = vec![0_u8; len as usize];
        let written = unsafe {
            host_session_get(
                key_bytes.as_ptr(),
                key_bytes.len() as i32,
                out.as_mut_ptr(),
                out.len() as i32,
            )
        };
        if written < 0 {
            return Err(written);
        }
        out.truncate(written as usize);
        Ok(Some(out))
    }

    pub fn session_put(key: &str, value: &[u8]) -> Result<(), i32> {
        let code = unsafe {
            host_session_put(
                key.as_bytes().as_ptr(),
                key.len() as i32,
                value.as_ptr(),
                value.len() as i32,
            )
        };
        if code < 0 {
            return Err(code);
        }
        Ok(())
    }

    pub fn tensor_session_has(
        key: &str,
        n_layers: usize,
        max_t: usize,
        kv_dim: usize,
    ) -> Result<bool, i32> {
        let code = unsafe {
            host_tensor_session_has(
                key.as_bytes().as_ptr(),
                key.len() as i32,
                n_layers as i32,
                max_t as i32,
                kv_dim as i32,
            )
        };
        if code < 0 {
            return Err(code);
        }
        Ok(code == 1)
    }

    pub fn tensor_session_reset(
        key: &str,
        n_layers: usize,
        max_t: usize,
        kv_dim: usize,
    ) -> Result<(), i32> {
        let code = unsafe {
            host_tensor_session_reset(
                key.as_bytes().as_ptr(),
                key.len() as i32,
                n_layers as i32,
                max_t as i32,
                kv_dim as i32,
            )
        };
        if code < 0 {
            return Err(code);
        }
        Ok(())
    }

    pub fn tensor_session_append_kv_fp(
        key: &str,
        layer_idx: usize,
        position: usize,
        k_src: &[f64],
        v_src: &[f64],
    ) -> Result<(), i32> {
        let code = unsafe {
            host_tensor_session_append_kv_fp(
                key.as_bytes().as_ptr(),
                key.len() as i32,
                layer_idx as i32,
                position as i32,
                k_src.as_ptr() as i32,
                v_src.as_ptr() as i32,
                k_src.len() as i32,
            )
        };
        if code < 0 {
            return Err(code);
        }
        Ok(())
    }

    pub fn tensor_session_attention_kv_fp(
        key: &str,
        q: &[f64],
        ctx: &mut [f64],
        t_total: usize,
        layer_idx: usize,
        n_q_heads: usize,
        n_kv_heads: usize,
        head_dim: usize,
    ) -> Result<(), i32> {
        let code = unsafe {
            host_tensor_session_attention_kv_fp(
                key.as_bytes().as_ptr(),
                key.len() as i32,
                q.as_ptr() as i32,
                ctx.as_mut_ptr() as i32,
                t_total as i32,
                layer_idx as i32,
                n_q_heads as i32,
                n_kv_heads as i32,
                head_dim as i32,
            )
        };
        if code < 0 {
            return Err(code);
        }
        Ok(())
    }

    pub fn tensor_load_i8_kv(
        dst: &mut [f64],
        key: &str,
        off: usize,
        n: usize,
        scale_bits: i64,
    ) -> Result<(), i32> {
        let code = unsafe {
            host_tensor_load_i8_kv(
                dst.as_mut_ptr() as i32,
                key.as_bytes().as_ptr(),
                key.len() as i32,
                off as i32,
                n as i32,
                scale_bits,
            )
        };
        if code < 0 {
            return Err(code);
        }
        Ok(())
    }

    pub fn tensor_matmul_fp(
        dst: &mut [f64],
        lhs: &[f64],
        rhs: &[f64],
        m: usize,
        k: usize,
        n: usize,
    ) -> Result<(), i32> {
        let code = unsafe {
            host_tensor_matmul_fp(
                dst.as_mut_ptr() as i32,
                lhs.as_ptr() as i32,
                rhs.as_ptr() as i32,
                m as i32,
                k as i32,
                n as i32,
            )
        };
        if code < 0 {
            return Err(code);
        }
        Ok(())
    }

    pub fn tensor_rmsnorm_fp(dst: &mut [f64], gamma: &[f64]) -> Result<(), i32> {
        let code = unsafe {
            host_tensor_rmsnorm_fp(
                dst.as_mut_ptr() as i32,
                dst.len() as i32,
                gamma.as_ptr() as i32,
            )
        };
        if code < 0 {
            return Err(code);
        }
        Ok(())
    }

    pub fn tensor_silu_fp(dst: &mut [f64]) -> Result<(), i32> {
        let code = unsafe { host_tensor_silu_fp(dst.as_mut_ptr() as i32, dst.len() as i32) };
        if code < 0 {
            return Err(code);
        }
        Ok(())
    }

    pub fn tensor_elemwise_mul_fp(dst: &mut [f64], src: &[f64]) -> Result<(), i32> {
        let code = unsafe {
            host_tensor_elemwise_mul_fp(
                dst.as_mut_ptr() as i32,
                src.as_ptr() as i32,
                dst.len() as i32,
            )
        };
        if code < 0 {
            return Err(code);
        }
        Ok(())
    }

    pub fn tensor_residual_add_fp(dst: &mut [f64], src: &[f64]) -> Result<(), i32> {
        let code = unsafe {
            host_tensor_residual_add_fp(
                dst.as_mut_ptr() as i32,
                src.as_ptr() as i32,
                dst.len() as i32,
            )
        };
        if code < 0 {
            return Err(code);
        }
        Ok(())
    }

    pub fn tensor_rope_apply_fp(dst: &mut [f64], position: i32, base_bits: i64) -> Result<(), i32> {
        let code = unsafe {
            host_tensor_rope_apply_fp(
                dst.as_mut_ptr() as i32,
                dst.len() as i32,
                position,
                base_bits,
            )
        };
        if code < 0 {
            return Err(code);
        }
        Ok(())
    }

    pub fn tensor_attention_kv_fp(
        q: &[f64],
        k: &[f64],
        v: &[f64],
        ctx: &mut [f64],
        t_total: usize,
        n_q_heads: usize,
        n_kv_heads: usize,
        head_dim: usize,
    ) -> Result<(), i32> {
        let code = unsafe {
            host_tensor_attention_kv_fp(
                q.as_ptr() as i32,
                k.as_ptr() as i32,
                v.as_ptr() as i32,
                ctx.as_mut_ptr() as i32,
                t_total as i32,
                n_q_heads as i32,
                n_kv_heads as i32,
                head_dim as i32,
            )
        };
        if code < 0 {
            return Err(code);
        }
        Ok(())
    }

    pub fn tensor_append_vec_fp(dst: &mut [f64], pos: usize, src: &[f64]) -> Result<(), i32> {
        let code = unsafe {
            host_tensor_append_vec_fp(
                dst.as_mut_ptr() as i32,
                pos as i32,
                src.as_ptr() as i32,
                src.len() as i32,
            )
        };
        if code < 0 {
            return Err(code);
        }
        Ok(())
    }

    pub fn tensor_argmax_fp(values: &[f64]) -> Result<i64, i32> {
        let code = unsafe { host_tensor_argmax_fp(values.as_ptr() as i32, values.len() as i32) };
        if code < 0 {
            return Err(code);
        }
        Ok(code as i64)
    }

    pub fn tensor_linear_i8_kv(
        out: &mut [f64],
        input: &[f64],
        key: &str,
        in_dim: usize,
        out_dim: usize,
        scale_bits: i64,
    ) -> Result<(), i32> {
        let code = unsafe {
            host_tensor_linear_i8_kv(
                out.as_mut_ptr() as i32,
                input.as_ptr() as i32,
                key.as_bytes().as_ptr(),
                key.len() as i32,
                in_dim as i32,
                out_dim as i32,
                scale_bits,
            )
        };
        if code < 0 {
            return Err(code);
        }
        Ok(())
    }

    pub fn tensor_argmax_i8_kv(
        input: &[f64],
        key: &str,
        rows: usize,
        cols: usize,
        scale_bits: i64,
    ) -> Result<i64, i32> {
        let code = unsafe {
            host_tensor_argmax_i8_kv(
                input.as_ptr() as i32,
                key.as_bytes().as_ptr(),
                key.len() as i32,
                rows as i32,
                cols as i32,
                scale_bits,
            )
        };
        if code < 0 {
            return Err(code);
        }
        Ok(code as i64)
    }

    pub fn tensor_top1_i8_kv(
        input: &[f64],
        key: &str,
        rows: usize,
        cols: usize,
        scale_bits: i64,
    ) -> Result<(i64, f64), i32> {
        let mut score = [0.0_f64; 1];
        let code = unsafe {
            host_tensor_top1_i8_kv(
                input.as_ptr() as i32,
                key.as_bytes().as_ptr(),
                key.len() as i32,
                rows as i32,
                cols as i32,
                scale_bits,
                score.as_mut_ptr() as i32,
            )
        };
        if code < 0 {
            return Err(code);
        }
        Ok((code as i64, score[0]))
    }

    pub fn emit_event(topic: &str, data: &[u8]) -> Result<(), i32> {
        let code = unsafe {
            host_emit_event(
                topic.as_bytes().as_ptr(),
                topic.len() as i32,
                data.as_ptr(),
                data.len() as i32,
            )
        };
        if code < 0 {
            return Err(code);
        }
        Ok(())
    }

    pub fn respond_value(value: Value) -> i32 {
        let payload = encode_value_frame(value);
        write_response_bytes(&payload)
    }

    pub fn respond_manifest_json(value: &str) -> i32 {
        write_response_bytes(value.as_bytes())
    }

    pub fn state_describe(state_ref: &str, input: &StateDescriptorInput<'_>) -> Result<bool, i32> {
        expect_bool(Self::circle_invoke(
            "state_describe",
            &[
                Value::String(state_ref.to_owned()),
                Value::String(input.state_class.as_str().to_owned()),
                Value::String(input.codec.to_owned()),
                optional_string_value(input.schema_hash),
                optional_string_value(input.subject_addr),
                Value::String(input.hfhe_profile.as_str().to_owned()),
                Value::Bool(input.mutable_state),
            ],
        )?)
    }

    pub fn state_publish(state_ref: &str, input: &StatePolicyInput<'_>) -> Result<bool, i32> {
        expect_bool(Self::circle_invoke(
            "state_publish",
            &[
                Value::String(state_ref.to_owned()),
                optional_string_value(input.delivery_key_id),
                Value::Int(input.activate_after_epoch.to_string()),
                Value::Int(input.expire_after_epoch.to_string()),
            ],
        )?)
    }

    pub fn state_release(state_ref: &str) -> Result<bool, i32> {
        expect_bool(Self::circle_invoke(
            "state_release",
            &[Value::String(state_ref.to_owned())],
        )?)
    }

    pub fn state_retire(state_ref: &str, expire_after_epoch: i64) -> Result<bool, i32> {
        expect_bool(Self::circle_invoke(
            "state_retire",
            &[
                Value::String(state_ref.to_owned()),
                Value::Int(expire_after_epoch.to_string()),
            ],
        )?)
    }

    pub fn state_tombstone_apply(state_ref: &str) -> Result<bool, i32> {
        expect_bool(Self::circle_invoke(
            "state_tombstone_apply",
            &[Value::String(state_ref.to_owned())],
        )?)
    }

    pub fn state_restore(state_ref: &str) -> Result<bool, i32> {
        expect_bool(Self::circle_invoke(
            "state_restore",
            &[Value::String(state_ref.to_owned())],
        )?)
    }

    pub fn state_revoke_apply(state_ref: &str) -> Result<bool, i32> {
        expect_bool(Self::circle_invoke(
            "state_revoke_apply",
            &[Value::String(state_ref.to_owned())],
        )?)
    }

    pub fn state_reinstate(state_ref: &str) -> Result<bool, i32> {
        expect_bool(Self::circle_invoke(
            "state_reinstate",
            &[Value::String(state_ref.to_owned())],
        )?)
    }

    pub fn state_descriptor_get(state_ref: &str, field: StateDescriptorField) -> Result<Value, i32> {
        Self::circle_invoke(
            "state_descriptor_get",
            &[
                Value::String(state_ref.to_owned()),
                Value::String(field.as_str().to_owned()),
            ],
        )
    }

    pub fn state_policy_get(state_ref: &str, field: StatePolicyField) -> Result<Value, i32> {
        Self::circle_invoke(
            "state_policy_get",
            &[
                Value::String(state_ref.to_owned()),
                Value::String(field.as_str().to_owned()),
            ],
        )
    }

    pub fn balance_cell_materialize(state_ref: &str, input: &BalanceCellInput<'_>) -> Result<bool, i32> {
        expect_bool(Self::circle_invoke(
            "balance_cell_materialize",
            &[
                Value::String(state_ref.to_owned()),
                Value::String(input.ciphertext_commitment.to_owned()),
                Value::String(input.amount_commitment.to_owned()),
                Value::String(input.proof_kind.as_str().to_owned()),
                optional_string_value(input.proof_receipt_hash),
            ],
        )?)
    }

    pub fn balance_cell_get(state_ref: &str, field: BalanceCellField) -> Result<Value, i32> {
        Self::circle_invoke(
            "balance_cell_get",
            &[
                Value::String(state_ref.to_owned()),
                Value::String(field.as_str().to_owned()),
            ],
        )
    }

    pub fn register_cell_materialize(state_ref: &str, input: &RegisterCellInput<'_>) -> Result<bool, i32> {
        expect_bool(Self::circle_invoke(
            "register_cell_materialize",
            &[
                Value::String(state_ref.to_owned()),
                Value::String(input.ciphertext_commitment.to_owned()),
                Value::String(input.proof_kind.as_str().to_owned()),
                optional_string_value(input.proof_receipt_hash),
            ],
        )?)
    }

    pub fn register_cell_get(state_ref: &str, field: RegisterCellField) -> Result<Value, i32> {
        Self::circle_invoke(
            "register_cell_get",
            &[
                Value::String(state_ref.to_owned()),
                Value::String(field.as_str().to_owned()),
            ],
        )
    }

    pub fn balance_binding_bind(subject_addr: &str, state_ref: &str, workflow_ref: &str, status: &str) -> Result<i64, i32> {
        expect_i64(Self::circle_invoke(
            "balance_binding_bind",
            &[
                Value::String(subject_addr.to_owned()),
                Value::String(state_ref.to_owned()),
                Value::String(workflow_ref.to_owned()),
                Value::String(status.to_owned()),
            ],
        )?)
    }

    pub fn balance_binding_get(subject_addr: &str, field: BalanceBindingField) -> Result<Value, i32> {
        Self::circle_invoke(
            "balance_binding_get",
            &[
                Value::String(subject_addr.to_owned()),
                Value::String(field.as_str().to_owned()),
            ],
        )
    }

    pub fn register_binding_bind(register_ref: &str, state_ref: &str, workflow_ref: &str, status: &str) -> Result<i64, i32> {
        expect_i64(Self::circle_invoke(
            "register_binding_bind",
            &[
                Value::String(register_ref.to_owned()),
                Value::String(state_ref.to_owned()),
                Value::String(workflow_ref.to_owned()),
                Value::String(status.to_owned()),
            ],
        )?)
    }

    pub fn register_binding_get(register_ref: &str, field: RegisterBindingField) -> Result<Value, i32> {
        Self::circle_invoke(
            "register_binding_get",
            &[
                Value::String(register_ref.to_owned()),
                Value::String(field.as_str().to_owned()),
            ],
        )
    }

    pub fn balance_workflow_record(workflow_ref: &str, input: &BalanceWorkflowInput<'_>) -> Result<bool, i32> {
        expect_bool(Self::circle_invoke(
            "balance_workflow_record",
            &[
                Value::String(workflow_ref.to_owned()),
                Value::String(input.flow_kind.to_owned()),
                Value::String(input.debit_subject_addr.to_owned()),
                Value::String(input.credit_subject_addr.to_owned()),
                Value::String(input.debit_state_ref.to_owned()),
                Value::String(input.credit_state_ref.to_owned()),
                Value::String(input.amount_commitment.to_owned()),
                Value::String(input.proof_kind.as_str().to_owned()),
                Value::String(input.proof_receipt_hash.to_owned()),
                Value::String(input.status.to_owned()),
                Value::String(input.intent_id.to_owned()),
            ],
        )?)
    }

    pub fn balance_workflow_get(workflow_ref: &str, field: BalanceWorkflowField) -> Result<Value, i32> {
        Self::circle_invoke(
            "balance_workflow_get",
            &[
                Value::String(workflow_ref.to_owned()),
                Value::String(field.as_str().to_owned()),
            ],
        )
    }

    pub fn register_workflow_record(workflow_ref: &str, input: &RegisterWorkflowInput<'_>) -> Result<bool, i32> {
        expect_bool(Self::circle_invoke(
            "register_workflow_record",
            &[
                Value::String(workflow_ref.to_owned()),
                Value::String(input.register_ref.to_owned()),
                Value::String(input.previous_state_ref.to_owned()),
                Value::String(input.next_state_ref.to_owned()),
                Value::String(input.workflow_kind.to_owned()),
                Value::String(input.proof_kind.as_str().to_owned()),
                Value::String(input.proof_receipt_hash.to_owned()),
                Value::String(input.status.to_owned()),
                Value::String(input.intent_id.to_owned()),
            ],
        )?)
    }

    pub fn register_workflow_get(workflow_ref: &str, field: RegisterWorkflowField) -> Result<Value, i32> {
        Self::circle_invoke(
            "register_workflow_get",
            &[
                Value::String(workflow_ref.to_owned()),
                Value::String(field.as_str().to_owned()),
            ],
        )
    }

    pub fn object_bind(object_ref: &str, state_ref: &str, transition_ref: &str, status: &str) -> Result<i64, i32> {
        expect_i64(Self::circle_invoke(
            "object_bind",
            &[
                Value::String(object_ref.to_owned()),
                Value::String(state_ref.to_owned()),
                Value::String(transition_ref.to_owned()),
                Value::String(status.to_owned()),
            ],
        )?)
    }

    pub fn object_binding_get(object_ref: &str, field: ObjectBindingField) -> Result<Value, i32> {
        Self::circle_invoke(
            "object_binding_get",
            &[
                Value::String(object_ref.to_owned()),
                Value::String(field.as_str().to_owned()),
            ],
        )
    }

    pub fn object_member_attach(object_ref: &str, member_ref: &str, input: &ObjectMemberInput<'_>) -> Result<bool, i32> {
        expect_bool(Self::circle_invoke(
            "object_member_attach",
            &[
                Value::String(object_ref.to_owned()),
                Value::String(member_ref.to_owned()),
                Value::String(input.state_ref.to_owned()),
                Value::String(input.member_kind.to_owned()),
                Value::String(input.state_class.as_str().to_owned()),
                Value::String(input.codec.to_owned()),
                Value::String(input.status.to_owned()),
            ],
        )?)
    }

    pub fn object_member_detach(object_ref: &str, member_ref: &str) -> Result<bool, i32> {
        expect_bool(Self::circle_invoke(
            "object_member_detach",
            &[
                Value::String(object_ref.to_owned()),
                Value::String(member_ref.to_owned()),
            ],
        )?)
    }

    pub fn object_member_get(object_ref: &str, member_ref: &str, field: ObjectMemberField) -> Result<Value, i32> {
        Self::circle_invoke(
            "object_member_get",
            &[
                Value::String(object_ref.to_owned()),
                Value::String(member_ref.to_owned()),
                Value::String(field.as_str().to_owned()),
            ],
        )
    }

    pub fn object_policy_define(object_ref: &str, input: &ObjectPolicyInput<'_>) -> Result<bool, i32> {
        expect_bool(Self::circle_invoke(
            "object_policy_define",
            &[
                Value::String(object_ref.to_owned()),
                optional_string_value(input.delivery_key_id),
                Value::Int(input.activate_after_epoch.to_string()),
                Value::Int(input.expire_after_epoch.to_string()),
                Value::String(input.transition_mode.as_str().to_owned()),
                Value::String(input.required_proof_kind.as_str().to_owned()),
                Value::Int(input.member_quorum.to_string()),
                Value::Bool(input.allow_detach),
                Value::Bool(input.allow_root_state_rotation),
            ],
        )?)
    }

    pub fn object_policy_release(object_ref: &str) -> Result<bool, i32> {
        expect_bool(Self::circle_invoke(
            "object_policy_release",
            &[Value::String(object_ref.to_owned())],
        )?)
    }

    pub fn object_policy_retire(object_ref: &str, expire_after_epoch: i64) -> Result<bool, i32> {
        expect_bool(Self::circle_invoke(
            "object_policy_retire",
            &[
                Value::String(object_ref.to_owned()),
                Value::Int(expire_after_epoch.to_string()),
            ],
        )?)
    }

    pub fn object_policy_tombstone(object_ref: &str) -> Result<bool, i32> {
        expect_bool(Self::circle_invoke(
            "object_policy_tombstone",
            &[Value::String(object_ref.to_owned())],
        )?)
    }

    pub fn object_policy_restore(object_ref: &str) -> Result<bool, i32> {
        expect_bool(Self::circle_invoke(
            "object_policy_restore",
            &[Value::String(object_ref.to_owned())],
        )?)
    }

    pub fn object_policy_revoke(object_ref: &str) -> Result<bool, i32> {
        expect_bool(Self::circle_invoke(
            "object_policy_revoke",
            &[Value::String(object_ref.to_owned())],
        )?)
    }

    pub fn object_policy_reinstate(object_ref: &str) -> Result<bool, i32> {
        expect_bool(Self::circle_invoke(
            "object_policy_reinstate",
            &[Value::String(object_ref.to_owned())],
        )?)
    }

    pub fn object_policy_get(object_ref: &str, field: ObjectPolicyField) -> Result<Value, i32> {
        Self::circle_invoke(
            "object_policy_get",
            &[
                Value::String(object_ref.to_owned()),
                Value::String(field.as_str().to_owned()),
            ],
        )
    }

    pub fn object_transition_record(input: &ObjectTransitionInput<'_>, touched_members_hash: &str) -> Result<bool, i32> {
        expect_bool(Self::circle_invoke(
            "object_transition_record",
            &[
                Value::String(input.transition_ref.to_owned()),
                Value::String(input.object_ref.to_owned()),
                Value::String(input.previous_state_ref.to_owned()),
                Value::String(input.next_state_ref.to_owned()),
                Value::String(touched_members_hash.to_owned()),
                Value::String(input.proof_kind.as_str().to_owned()),
                Value::String(input.proof_receipt_hash.to_owned()),
                Value::String(input.status.to_owned()),
                Value::String(input.intent_id.to_owned()),
            ],
        )?)
    }

    pub fn object_transition_apply(
        input: &ObjectTransitionInput<'_>,
        member_deltas: &[ObjectMemberDelta<'_>],
    ) -> Result<i64, i32> {
        let member_bundle = encode_object_member_bundle(member_deltas)?;
        let touched_members_hash = sha256_hex(member_bundle.as_bytes());
        expect_i64(Self::circle_invoke(
            "object_transition_apply",
            &[
                Value::String(input.transition_ref.to_owned()),
                Value::String(input.object_ref.to_owned()),
                Value::String(input.previous_state_ref.to_owned()),
                Value::String(input.next_state_ref.to_owned()),
                Value::String(member_bundle),
                Value::String(touched_members_hash),
                Value::String(input.proof_kind.as_str().to_owned()),
                Value::String(input.proof_receipt_hash.to_owned()),
                Value::String(input.status.to_owned()),
                Value::String(input.intent_id.to_owned()),
            ],
        )?)
    }

    pub fn prepare_balance_room(input: &BalanceRoomInput<'_>) -> Result<i64, i32> {
        Self::state_describe(input.state_ref, &input.descriptor)?;
        Self::state_publish(input.state_ref, &input.policy)?;
        Self::balance_cell_materialize(input.state_ref, &input.cell)?;
        Self::balance_binding_bind(
            input.subject_addr,
            input.state_ref,
            input.workflow_ref,
            input.binding_status,
        )?;
        Self::balance_workflow_record(input.workflow_ref, &input.workflow)?;
        Self::object_policy_define(input.object_ref, &input.object_policy)?;
        let version = Self::object_bind(
            input.object_ref,
            input.state_ref,
            input.object_transition_ref,
            input.object_status,
        )?;
        Self::object_member_attach(
            input.object_ref,
            input.object_member_ref,
            &input.object_member,
        )?;
        Ok(version)
    }

    pub fn prepare_register_lane(input: &RegisterLaneInput<'_>) -> Result<i64, i32> {
        Self::state_describe(input.state_ref, &input.descriptor)?;
        Self::register_cell_materialize(input.state_ref, &input.cell)?;
        let version = Self::register_binding_bind(
            input.register_ref,
            input.state_ref,
            input.workflow_ref,
            input.binding_status,
        )?;
        Self::register_workflow_record(input.workflow_ref, &input.workflow)?;
        Ok(version)
    }

    pub fn fhe_pedersen(amount: i64, blinding_b64: &str) -> Result<String, i32> {
        expect_string(Self::hfhe_invoke(
            "fhe_pedersen",
            &[
                Value::Int(amount.to_string()),
                Value::String(blinding_b64.to_owned()),
            ],
        )?)
    }

    pub fn fhe_load_pk(requested_addr: &str) -> Result<String, i32> {
        expect_string(Self::hfhe_invoke(
            "fhe_load_pk",
            &[Value::String(requested_addr.to_owned())],
        )?)
    }

    pub fn fhe_encrypt(amount: i64, seed_b64: &str) -> Result<String, i32> {
        expect_string(Self::hfhe_invoke(
            "fhe_encrypt",
            &[
                Value::Int(amount.to_string()),
                Value::String(seed_b64.to_owned()),
            ],
        )?)
    }

    pub fn fhe_encrypt_zero(seed_b64: &str) -> Result<String, i32> {
        expect_string(Self::hfhe_invoke(
            "fhe_encrypt_zero",
            &[Value::String(seed_b64.to_owned())],
        )?)
    }

    pub fn fhe_decrypt(ciphertext: &str) -> Result<i64, i32> {
        expect_i64(Self::hfhe_invoke(
            "fhe_decrypt",
            &[Value::String(ciphertext.to_owned())],
        )?)
    }

    pub fn fhe_add(pubkey_b64: &str, lhs_ciphertext: &str, rhs_ciphertext: &str) -> Result<String, i32> {
        expect_string(Self::hfhe_invoke(
            "fhe_add",
            &[
                Value::String(pubkey_b64.to_owned()),
                Value::String(lhs_ciphertext.to_owned()),
                Value::String(rhs_ciphertext.to_owned()),
            ],
        )?)
    }

    pub fn fhe_sub(pubkey_b64: &str, lhs_ciphertext: &str, rhs_ciphertext: &str) -> Result<String, i32> {
        expect_string(Self::hfhe_invoke(
            "fhe_sub",
            &[
                Value::String(pubkey_b64.to_owned()),
                Value::String(lhs_ciphertext.to_owned()),
                Value::String(rhs_ciphertext.to_owned()),
            ],
        )?)
    }

    pub fn fhe_scale(pubkey_b64: &str, ciphertext: &str, factor: i64) -> Result<String, i32> {
        expect_string(Self::hfhe_invoke(
            "fhe_scale",
            &[
                Value::String(pubkey_b64.to_owned()),
                Value::String(ciphertext.to_owned()),
                Value::Int(factor.to_string()),
            ],
        )?)
    }

    pub fn fhe_add_const(pubkey_b64: &str, ciphertext: &str, amount: i64) -> Result<String, i32> {
        expect_string(Self::hfhe_invoke(
            "fhe_add_const",
            &[
                Value::String(pubkey_b64.to_owned()),
                Value::String(ciphertext.to_owned()),
                Value::Int(amount.to_string()),
            ],
        )?)
    }

    pub fn fhe_sub_const(pubkey_b64: &str, ciphertext: &str, amount: i64) -> Result<String, i32> {
        expect_string(Self::hfhe_invoke(
            "fhe_sub_const",
            &[
                Value::String(pubkey_b64.to_owned()),
                Value::String(ciphertext.to_owned()),
                Value::Int(amount.to_string()),
            ],
        )?)
    }

    pub fn fhe_commit(pubkey_b64: &str, ciphertext: &str) -> Result<String, i32> {
        expect_string(Self::hfhe_invoke(
            "fhe_commit",
            &[
                Value::String(pubkey_b64.to_owned()),
                Value::String(ciphertext.to_owned()),
            ],
        )?)
    }

    pub fn fhe_bound_commitment(pubkey_b64: &str, ciphertext: &str, amount: i64) -> Result<String, i32> {
        expect_string(Self::hfhe_invoke(
            "fhe_bound_commitment",
            &[
                Value::String(pubkey_b64.to_owned()),
                Value::String(ciphertext.to_owned()),
                Value::Int(amount.to_string()),
            ],
        )?)
    }

    pub fn fhe_verify_zero(pubkey_b64: &str, ciphertext: &str, proof: &str) -> Result<bool, i32> {
        expect_bool(Self::hfhe_invoke(
            "fhe_verify_zero",
            &[
                Value::String(pubkey_b64.to_owned()),
                Value::String(ciphertext.to_owned()),
                Value::String(proof.to_owned()),
            ],
        )?)
    }

    pub fn fhe_verify_range(
        pubkey_b64: &str,
        ciphertext: &str,
        proof: &str,
        amount_commitment: &str,
    ) -> Result<bool, i32> {
        expect_bool(Self::hfhe_invoke(
            "fhe_verify_range",
            &[
                Value::String(pubkey_b64.to_owned()),
                Value::String(ciphertext.to_owned()),
                Value::String(proof.to_owned()),
                Value::String(amount_commitment.to_owned()),
            ],
        )?)
    }

    pub fn fhe_verify_bound(pubkey_b64: &str, ciphertext: &str, proof: &str, amount_commitment: &str) -> Result<bool, i32> {
        expect_bool(Self::hfhe_invoke(
            "fhe_verify_bound",
            &[
                Value::String(pubkey_b64.to_owned()),
                Value::String(ciphertext.to_owned()),
                Value::String(proof.to_owned()),
                Value::String(amount_commitment.to_owned()),
            ],
        )?)
    }

    fn circle_invoke(method: &str, params: &[Value]) -> Result<Value, i32> {
        let request = encode_request_frame(method, params);
        let response_len = unsafe { host_circle_invoke_len(request.as_ptr(), request.len() as i32) };
        if response_len < 0 {
            return Err(response_len);
        }
        let mut out = vec![0_u8; response_len as usize];
        let written = unsafe {
            host_circle_invoke(
                request.as_ptr(),
                request.len() as i32,
                out.as_mut_ptr(),
                out.len() as i32,
            )
        };
        if written < 0 {
            return Err(written);
        }
        out.truncate(written as usize);
        decode_response_value(&out)
    }

    fn hfhe_invoke(method: &str, params: &[Value]) -> Result<Value, i32> {
        let request = encode_request_frame(method, params);
        let response_len = unsafe { host_hfhe_invoke_len(request.as_ptr(), request.len() as i32) };
        if response_len < 0 {
            return Err(response_len);
        }
        let mut out = vec![0_u8; response_len as usize];
        let written = unsafe {
            host_hfhe_invoke(
                request.as_ptr(),
                request.len() as i32,
                out.as_mut_ptr(),
                out.len() as i32,
            )
        };
        if written < 0 {
            return Err(written);
        }
        out.truncate(written as usize);
        decode_response_value(&out)
    }
}

#[no_mangle]
pub extern "C" fn octra_alloc(len: i32) -> i32 {
    if len <= 0 {
        return 0;
    }
    let mut bytes = Vec::<u8>::with_capacity(len as usize);
    let ptr = bytes.as_mut_ptr();
    std::mem::forget(bytes);
    ptr as i32
}

pub fn decode_request(ptr: i32, len: i32) -> Result<Request, i32> {
    if ptr < 0 || len < 0 {
        return Err(30);
    }
    let bytes = unsafe { std::slice::from_raw_parts(ptr as *const u8, len as usize) };
    Request::decode(bytes)
}

fn encode_object_member_bundle(member_deltas: &[ObjectMemberDelta<'_>]) -> Result<String, i32> {
    let mut entries = Vec::with_capacity(member_deltas.len());
    for delta in member_deltas {
        match delta {
            ObjectMemberDelta::Attach { member_ref, input } => {
                validate_bundle_atom("member_ref", member_ref)?;
                validate_bundle_atom("state_ref", input.state_ref)?;
                validate_bundle_atom("member_kind", input.member_kind)?;
                validate_bundle_atom("state_class", input.state_class.as_str())?;
                validate_bundle_atom("codec", input.codec)?;
                validate_bundle_atom("status", input.status)?;
                entries.push(format!(
                    "attach|{}|{}|{}|{}|{}|{}",
                    member_ref,
                    input.state_ref,
                    input.member_kind,
                    input.state_class.as_str(),
                    input.codec,
                    input.status
                ));
            }
            ObjectMemberDelta::Detach { member_ref } => {
                validate_bundle_atom("member_ref", member_ref)?;
                entries.push(format!("detach|{}", member_ref));
            }
        }
    }
    Ok(entries.join(";"))
}

fn validate_bundle_atom(_label: &str, value: &str) -> Result<(), i32> {
    if value.trim().is_empty() || value.contains('|') || value.contains(';') {
        Err(73)
    } else {
        Ok(())
    }
}

fn optional_string_value(value: Option<&str>) -> Value {
    match value {
        Some(value) => Value::String(value.to_owned()),
        None => Value::Null,
    }
}

fn expect_bool(value: Value) -> Result<bool, i32> {
    value.as_bool().ok_or(70)
}

fn expect_i64(value: Value) -> Result<i64, i32> {
    value.as_i64().ok_or(71)
}

fn expect_string(value: Value) -> Result<String, i32> {
    match value {
        Value::String(value) | Value::Int(value) => Ok(value),
        Value::Null | Value::Bool(_) => Err(72),
    }
}

fn write_response_bytes(bytes: &[u8]) -> i32 {
    let _ = unsafe { host_response_reset() };
    let write_code = unsafe { host_response_write(bytes.as_ptr(), bytes.len() as i32) };
    if write_code < 0 {
        return 40;
    }
    let finish_code = unsafe { host_response_finish(0) };
    if finish_code < 0 {
        return 41;
    }
    0
}

fn encode_request_frame(method: &str, params: &[Value]) -> Vec<u8> {
    let mut out = Vec::with_capacity(REQUEST_MAGIC.len() + method.len() + params.len() * 8);
    out.extend_from_slice(REQUEST_MAGIC);
    out.extend_from_slice(&(method.len() as u16).to_be_bytes());
    out.extend_from_slice(method.as_bytes());
    out.extend_from_slice(&(params.len() as u16).to_be_bytes());
    for value in params {
        match value {
            Value::Null => {
                out.push(0);
                out.extend_from_slice(&0_u32.to_be_bytes());
            }
            Value::Bool(false) => {
                out.push(1);
                out.extend_from_slice(&0_u32.to_be_bytes());
            }
            Value::Bool(true) => {
                out.push(2);
                out.extend_from_slice(&0_u32.to_be_bytes());
            }
            Value::Int(value) => {
                out.push(3);
                out.extend_from_slice(&(value.len() as u32).to_be_bytes());
                out.extend_from_slice(value.as_bytes());
            }
            Value::String(value) => {
                out.push(4);
                out.extend_from_slice(&(value.len() as u32).to_be_bytes());
                out.extend_from_slice(value.as_bytes());
            }
        }
    }
    out
}

fn encode_value_frame(value: Value) -> Vec<u8> {
    let (tag, payload) = match value {
        Value::Null => (0_u8, Vec::new()),
        Value::Bool(false) => (1_u8, Vec::new()),
        Value::Bool(true) => (2_u8, Vec::new()),
        Value::Int(value) => (3_u8, value.into_bytes()),
        Value::String(value) => (4_u8, value.into_bytes()),
    };
    let mut out = Vec::with_capacity(RESPONSE_MAGIC.len() + 5 + payload.len());
    out.extend_from_slice(RESPONSE_MAGIC);
    out.push(tag);
    out.extend_from_slice(&(payload.len() as u32).to_be_bytes());
    out.extend_from_slice(&payload);
    out
}

fn decode_response_value(raw: &[u8]) -> Result<Value, i32> {
    if raw.len() < RESPONSE_MAGIC.len() + 5 {
        return Err(42);
    }
    if &raw[..RESPONSE_MAGIC.len()] != RESPONSE_MAGIC {
        return Err(43);
    }
    let tag = raw[RESPONSE_MAGIC.len()];
    let payload_len = u32::from_be_bytes([
        raw[RESPONSE_MAGIC.len() + 1],
        raw[RESPONSE_MAGIC.len() + 2],
        raw[RESPONSE_MAGIC.len() + 3],
        raw[RESPONSE_MAGIC.len() + 4],
    ]) as usize;
    let payload_offset = RESPONSE_MAGIC.len() + 5;
    if payload_offset + payload_len != raw.len() {
        return Err(44);
    }
    let payload = &raw[payload_offset..];
    match tag {
        0 => Ok(Value::Null),
        1 => Ok(Value::Bool(false)),
        2 => Ok(Value::Bool(true)),
        3 => String::from_utf8(payload.to_vec()).map(Value::Int).map_err(|_| 45),
        4 => String::from_utf8(payload.to_vec()).map(Value::String).map_err(|_| 46),
        _ => Err(47),
    }
}

fn read_len_prefixed(
    len_fn: unsafe extern "C" fn() -> i32,
    read_fn: unsafe extern "C" fn(i32, i32) -> i32,
) -> Result<String, i32> {
    let len = unsafe { len_fn() };
    if len < 0 {
        return Err(len);
    }
    let mut out = vec![0_u8; len as usize];
    let written = unsafe { read_fn(out.as_mut_ptr() as i32, out.len() as i32) };
    if written < 0 {
        return Err(written);
    }
    out.truncate(written as usize);
    String::from_utf8(out).map_err(|_| 25)
}

fn decode_u8(raw: &[u8], offset: &mut usize) -> Result<u8, i32> {
    if *offset >= raw.len() {
        return Err(15);
    }
    let value = raw[*offset];
    *offset += 1;
    Ok(value)
}

fn decode_u16(raw: &[u8], offset: &mut usize) -> Result<u16, i32> {
    let bytes = decode_bytes(raw, offset, 2)?;
    Ok(u16::from_be_bytes([bytes[0], bytes[1]]))
}

fn decode_u32(raw: &[u8], offset: &mut usize) -> Result<u32, i32> {
    let bytes = decode_bytes(raw, offset, 4)?;
    Ok(u32::from_be_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]))
}

fn decode_string(raw: &[u8], offset: &mut usize, len: usize) -> Result<String, i32> {
    let bytes = decode_bytes(raw, offset, len)?;
    String::from_utf8(bytes.to_vec()).map_err(|_| 16)
}

fn decode_bytes<'a>(raw: &'a [u8], offset: &mut usize, len: usize) -> Result<&'a [u8], i32> {
    if *offset + len > raw.len() {
        return Err(17);
    }
    let value = &raw[*offset..*offset + len];
    *offset += len;
    Ok(value)
}

fn sha256_hex(input: &[u8]) -> String {
    const H0: [u32; 8] = [
        0x6a09e667,
        0xbb67ae85,
        0x3c6ef372,
        0xa54ff53a,
        0x510e527f,
        0x9b05688c,
        0x1f83d9ab,
        0x5be0cd19,
    ];
    const K: [u32; 64] = [
        0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
        0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
        0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
        0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
        0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
        0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
        0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
        0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
        0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
        0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
        0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
        0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
        0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
        0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
        0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
        0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
    ];

    let mut msg = input.to_vec();
    let bit_len = (msg.len() as u64) * 8;
    msg.push(0x80);
    while (msg.len() % 64) != 56 {
        msg.push(0);
    }
    msg.extend_from_slice(&bit_len.to_be_bytes());

    let mut h = H0;
    let mut w = [0_u32; 64];

    for chunk in msg.chunks_exact(64) {
        for (i, word) in w.iter_mut().take(16).enumerate() {
            let j = i * 4;
            *word = u32::from_be_bytes([chunk[j], chunk[j + 1], chunk[j + 2], chunk[j + 3]]);
        }
        for i in 16..64 {
            let s0 = w[i - 15].rotate_right(7) ^ w[i - 15].rotate_right(18) ^ (w[i - 15] >> 3);
            let s1 = w[i - 2].rotate_right(17) ^ w[i - 2].rotate_right(19) ^ (w[i - 2] >> 10);
            w[i] = w[i - 16]
                .wrapping_add(s0)
                .wrapping_add(w[i - 7])
                .wrapping_add(s1);
        }

        let mut a = h[0];
        let mut b = h[1];
        let mut c = h[2];
        let mut d = h[3];
        let mut e = h[4];
        let mut f = h[5];
        let mut g = h[6];
        let mut hh = h[7];

        for i in 0..64 {
            let s1 = e.rotate_right(6) ^ e.rotate_right(11) ^ e.rotate_right(25);
            let ch = (e & f) ^ ((!e) & g);
            let temp1 = hh
                .wrapping_add(s1)
                .wrapping_add(ch)
                .wrapping_add(K[i])
                .wrapping_add(w[i]);
            let s0 = a.rotate_right(2) ^ a.rotate_right(13) ^ a.rotate_right(22);
            let maj = (a & b) ^ (a & c) ^ (b & c);
            let temp2 = s0.wrapping_add(maj);

            hh = g;
            g = f;
            f = e;
            e = d.wrapping_add(temp1);
            d = c;
            c = b;
            b = a;
            a = temp1.wrapping_add(temp2);
        }

        h[0] = h[0].wrapping_add(a);
        h[1] = h[1].wrapping_add(b);
        h[2] = h[2].wrapping_add(c);
        h[3] = h[3].wrapping_add(d);
        h[4] = h[4].wrapping_add(e);
        h[5] = h[5].wrapping_add(f);
        h[6] = h[6].wrapping_add(g);
        h[7] = h[7].wrapping_add(hh);
    }

    let mut out = String::with_capacity(64);
    for word in h {
        out.push(nibble_hex(((word >> 28) & 0xF) as u8));
        out.push(nibble_hex(((word >> 24) & 0xF) as u8));
        out.push(nibble_hex(((word >> 20) & 0xF) as u8));
        out.push(nibble_hex(((word >> 16) & 0xF) as u8));
        out.push(nibble_hex(((word >> 12) & 0xF) as u8));
        out.push(nibble_hex(((word >> 8) & 0xF) as u8));
        out.push(nibble_hex(((word >> 4) & 0xF) as u8));
        out.push(nibble_hex((word & 0xF) as u8));
    }
    out
}

fn nibble_hex(value: u8) -> char {
    match value {
        0..=9 => (b'0' + value) as char,
        10..=15 => (b'a' + (value - 10)) as char,
        _ => '0',
    }
}

#[link(wasm_import_module = "octra")]
extern "C" {
    fn host_response_reset() -> i32;
    fn host_response_write(ptr: *const u8, len: i32) -> i32;
    fn host_response_finish(status_code: i32) -> i32;

    fn host_circle_invoke_len(req_ptr: *const u8, req_len: i32) -> i32;
    fn host_circle_invoke(req_ptr: *const u8, req_len: i32, out_ptr: *mut u8, out_cap: i32) -> i32;
    fn host_hfhe_invoke_len(req_ptr: *const u8, req_len: i32) -> i32;
    fn host_hfhe_invoke(req_ptr: *const u8, req_len: i32, out_ptr: *mut u8, out_cap: i32) -> i32;

    fn host_kv_get_len(key_ptr: *const u8, key_len: i32) -> i32;
    fn host_kv_get(key_ptr: *const u8, key_len: i32, out_ptr: *mut u8, out_cap: i32) -> i32;
    fn host_kv_put(key_ptr: *const u8, key_len: i32, value_ptr: *const u8, value_len: i32) -> i32;
    fn host_kv_del(key_ptr: *const u8, key_len: i32) -> i32;
    fn host_session_get_len(key_ptr: *const u8, key_len: i32) -> i32;
    fn host_session_get(key_ptr: *const u8, key_len: i32, out_ptr: *mut u8, out_cap: i32) -> i32;
    fn host_session_put(key_ptr: *const u8, key_len: i32, value_ptr: *const u8, value_len: i32) -> i32;
    fn host_tensor_session_has(
        key_ptr: *const u8,
        key_len: i32,
        n_layers: i32,
        max_t: i32,
        kv_dim: i32,
    ) -> i32;
    fn host_tensor_session_reset(
        key_ptr: *const u8,
        key_len: i32,
        n_layers: i32,
        max_t: i32,
        kv_dim: i32,
    ) -> i32;
    fn host_tensor_session_append_kv_fp(
        key_ptr: *const u8,
        key_len: i32,
        layer_idx: i32,
        position: i32,
        k_ptr: i32,
        v_ptr: i32,
        n: i32,
    ) -> i32;
    fn host_tensor_session_attention_kv_fp(
        key_ptr: *const u8,
        key_len: i32,
        q_ptr: i32,
        ctx_ptr: i32,
        t_total: i32,
        layer_idx: i32,
        n_q_heads: i32,
        n_kv_heads: i32,
        head_dim: i32,
    ) -> i32;

    fn host_epoch() -> i64;
    fn host_caller_len() -> i32;
    fn host_caller_read(out_ptr: i32, out_cap: i32) -> i32;
    fn host_self_len() -> i32;
    fn host_self_read(out_ptr: i32, out_cap: i32) -> i32;
    fn host_state_path_key_len(state_ref_ptr: *const u8, state_ref_len: i32) -> i32;
    fn host_state_path_key(
        state_ref_ptr: *const u8,
        state_ref_len: i32,
        out_ptr: *mut u8,
        out_cap: i32,
    ) -> i32;
    fn host_emit_event(topic_ptr: *const u8, topic_len: i32, data_ptr: *const u8, data_len: i32) -> i32;
    fn host_tensor_load_i8_kv(
        dst_ptr: i32,
        key_ptr: *const u8,
        key_len: i32,
        off: i32,
        n: i32,
        scale_bits: i64,
    ) -> i32;
    fn host_tensor_matmul_fp(
        dst_ptr: i32,
        lhs_ptr: i32,
        rhs_ptr: i32,
        m: i32,
        k: i32,
        n: i32,
    ) -> i32;
    fn host_tensor_rmsnorm_fp(addr_ptr: i32, n: i32, gamma_ptr: i32) -> i32;
    fn host_tensor_silu_fp(addr_ptr: i32, n: i32) -> i32;
    fn host_tensor_elemwise_mul_fp(dst_ptr: i32, src_ptr: i32, n: i32) -> i32;
    fn host_tensor_residual_add_fp(dst_ptr: i32, src_ptr: i32, n: i32) -> i32;
    fn host_tensor_rope_apply_fp(addr_ptr: i32, n_dim: i32, pos: i32, base_bits: i64) -> i32;
    fn host_tensor_attention_kv_fp(
        q_ptr: i32,
        k_ptr: i32,
        v_ptr: i32,
        ctx_ptr: i32,
        t_total: i32,
        n_q_heads: i32,
        n_kv_heads: i32,
        head_dim: i32,
    ) -> i32;
    fn host_tensor_append_vec_fp(dst_ptr: i32, pos: i32, src_ptr: i32, n: i32) -> i32;
    fn host_tensor_argmax_fp(addr_ptr: i32, n: i32) -> i32;
    fn host_tensor_linear_i8_kv(
        out_ptr: i32,
        in_ptr: i32,
        key_ptr: *const u8,
        key_len: i32,
        in_dim: i32,
        out_dim: i32,
        scale_bits: i64,
    ) -> i32;
    fn host_tensor_argmax_i8_kv(
        in_ptr: i32,
        key_ptr: *const u8,
        key_len: i32,
        rows: i32,
        cols: i32,
        scale_bits: i64,
    ) -> i32;
    fn host_tensor_top1_i8_kv(
        in_ptr: i32,
        key_ptr: *const u8,
        key_len: i32,
        rows: i32,
        cols: i32,
        scale_bits: i64,
        score_out_ptr: i32,
    ) -> i32;
}