// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

// Prepared by Denis CMIX for the Octra network.
// Use, modify, adapt, and redistribute this example as you see fit.

use rust_circle_sdk::{
    BalanceRoomInput,
    decode_request,
    BalanceBindingField,
    BalanceCellField,
    BalanceCellInput,
    BalanceWorkflowField,
    BalanceWorkflowInput,
    Host,
    HfheProfile,
    ObjectMemberDelta,
    ObjectBindingField,
    ObjectMemberField,
    ObjectMemberInput,
    ObjectPolicyField,
    ObjectPolicyInput,
    ObjectTransitionInput,
    ProofKind,
    RegisterLaneInput,
    RegisterBindingField,
    RegisterCellField,
    RegisterCellInput,
    RegisterWorkflowField,
    RegisterWorkflowInput,
    StateClass,
    StateDescriptorField,
    StateDescriptorInput,
    StatePolicyField,
    StatePolicyInput,
    TransitionMode,
    Value,
};

fn load_counter() -> Result<i64, i32> {
    Ok(
        Host::kv_get_string("counter")?
            .unwrap_or_else(|| String::from("0"))
            .parse::<i64>()
            .map_err(|_| 60)?
    )
}

fn store_counter(value: i64) -> Result<(), i32> {
    Host::kv_put_string("counter", &value.to_string())
}

fn respond_manifest() -> i32 {
    Host::respond_manifest_json(
        "{\"methods\":[{\"name\":\"get_counter\",\"view\":true},{\"name\":\"inc\",\"view\":false},{\"name\":\"caller_echo\",\"view\":true},{\"name\":\"state_path_key_of\",\"view\":true},{\"name\":\"runtime_guard\",\"view\":true},{\"name\":\"state_class_of\",\"view\":true},{\"name\":\"state_delivery_key_of\",\"view\":true},{\"name\":\"balance_commitment_of\",\"view\":true},{\"name\":\"balance_workflow_status_of\",\"view\":true},{\"name\":\"object_status_of\",\"view\":true},{\"name\":\"object_last_transition_of\",\"view\":true},{\"name\":\"object_member_status_of\",\"view\":true},{\"name\":\"object_quorum_of\",\"view\":true},{\"name\":\"binding_version_of\",\"view\":true},{\"name\":\"register_proof_kind_of\",\"view\":true},{\"name\":\"register_binding_version_of\",\"view\":true},{\"name\":\"register_workflow_status_of\",\"view\":true},{\"name\":\"fhe_commit_loaded_pk_of\",\"view\":true},{\"name\":\"fhe_encrypt_of\",\"view\":true},{\"name\":\"fhe_encrypt_zero_of\",\"view\":true},{\"name\":\"fhe_decrypt_of\",\"view\":true},{\"name\":\"fhe_add_of\",\"view\":true},{\"name\":\"fhe_sub_of\",\"view\":true},{\"name\":\"fhe_scale_of\",\"view\":true},{\"name\":\"fhe_add_const_of\",\"view\":true},{\"name\":\"fhe_sub_const_of\",\"view\":true},{\"name\":\"fhe_pedersen_of\",\"view\":true},{\"name\":\"fhe_commit_of\",\"view\":true},{\"name\":\"fhe_bound_commitment_of\",\"view\":true},{\"name\":\"fhe_verify_zero_of\",\"view\":true},{\"name\":\"fhe_verify_range_of\",\"view\":true},{\"name\":\"fhe_verify_bound_of\",\"view\":true},{\"name\":\"fhe_verify_zero_and_record\",\"view\":false},{\"name\":\"public_asset_read\",\"view\":true,\"public_reads\":[{\"circle_id\":\"oct11111111111111111111111111111111111111111111\",\"path\":\"/public.bin\",\"offset\":2,\"max_bytes\":4}]},{\"name\":\"public_asset_record\",\"view\":false,\"public_reads\":[{\"circle_id\":\"oct11111111111111111111111111111111111111111111\",\"path\":\"/public.bin\",\"offset\":2,\"max_bytes\":4}]},{\"name\":\"public_asset_undeclared\",\"view\":true},{\"name\":\"prepare_room\",\"view\":false},{\"name\":\"prepare_register_lane\",\"view\":false},{\"name\":\"promote_room\",\"view\":false},{\"name\":\"kernel_probe\",\"view\":false}]}"
    )
}

fn string_result(value: Result<Value, i32>) -> i32 {
    match value {
        Ok(value) => Host::respond_value(value),
        Err(code) => code,
    }
}

fn prepare_room(request: &rust_circle_sdk::Request) -> i32 {
    let state_ref = match request.string_param(0) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let object_ref = match request.string_param(1) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let member_ref = match request.string_param(2) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let subject_addr = match request.string_param(3) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let workflow_ref = match request.string_param(4) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let transition_ref = match request.string_param(5) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let ciphertext_commitment = match request.string_param(6) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let amount_commitment = match request.string_param(7) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let proof_receipt_hash = match request.string_param(8) {
        Ok(value) => value,
        Err(code) => return code,
    };

    let room = BalanceRoomInput {
        state_ref,
        descriptor: StateDescriptorInput {
            state_class: StateClass::BalanceCell,
            codec: "balance_cell_v1",
            schema_hash: None,
            subject_addr: Some(subject_addr),
            hfhe_profile: HfheProfile::CommitBoundV1,
            mutable_state: true,
        },
        policy: StatePolicyInput {
            delivery_key_id: Some("room-key"),
            activate_after_epoch: 2,
            expire_after_epoch: 90,
        },
        cell: BalanceCellInput {
            ciphertext_commitment,
            amount_commitment,
            proof_kind: ProofKind::BoundZeroReceiptV1,
            proof_receipt_hash: Some(proof_receipt_hash),
        },
        subject_addr,
        workflow_ref,
        binding_status: "ready",
        workflow: BalanceWorkflowInput {
            flow_kind: "rebalance",
            debit_subject_addr: subject_addr,
            credit_subject_addr: subject_addr,
            debit_state_ref: state_ref,
            credit_state_ref: state_ref,
            amount_commitment,
            proof_kind: ProofKind::BoundZeroReceiptV1,
            proof_receipt_hash,
            status: "ready",
            intent_id: transition_ref,
        },
        object_ref,
        object_transition_ref: transition_ref,
        object_status: "assembling",
        object_policy: ObjectPolicyInput {
            delivery_key_id: Some("room-key"),
            activate_after_epoch: 2,
            expire_after_epoch: 90,
            transition_mode: TransitionMode::ProofRequired,
            required_proof_kind: ProofKind::BoundZeroReceiptV1,
            member_quorum: 2,
            allow_detach: false,
            allow_root_state_rotation: false,
        },
        object_member_ref: member_ref,
        object_member: ObjectMemberInput {
            state_ref,
            member_kind: "treasury",
            state_class: StateClass::BalanceCell,
            codec: "balance_cell_v1",
            status: "live",
        },
    };

    match Host::prepare_balance_room(&room) {
        Ok(version) => Host::respond_value(Value::Int(version.to_string())),
        Err(code) => code,
    }
}

fn prepare_register_lane(request: &rust_circle_sdk::Request) -> i32 {
    let state_ref = match request.string_param(0) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let register_ref = match request.string_param(1) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let workflow_ref = match request.string_param(2) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let ciphertext_commitment = match request.string_param(3) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let proof_receipt_hash = match request.string_param(4) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let next_state_ref = match request.string_param(5) {
        Ok(value) => value,
        Err(code) => return code,
    };

    let lane = RegisterLaneInput {
        state_ref,
        descriptor: StateDescriptorInput {
            state_class: StateClass::RegisterCell,
            codec: "register_cell_v1",
            schema_hash: None,
            subject_addr: None,
            hfhe_profile: HfheProfile::RegisterCellV1,
            mutable_state: true,
        },
        cell: RegisterCellInput {
            ciphertext_commitment,
            proof_kind: ProofKind::ZeroReceiptV1,
            proof_receipt_hash: Some(proof_receipt_hash),
        },
        register_ref,
        workflow_ref,
        binding_status: "ready",
        workflow: RegisterWorkflowInput {
            register_ref,
            previous_state_ref: state_ref,
            next_state_ref,
            workflow_kind: "rotate",
            proof_kind: ProofKind::ZeroReceiptV1,
            proof_receipt_hash,
            status: "ready",
            intent_id: workflow_ref,
        },
    };

    match Host::prepare_register_lane(&lane) {
        Ok(version) => Host::respond_value(Value::Int(version.to_string())),
        Err(code) => code,
    }
}

fn promote_room(request: &rust_circle_sdk::Request) -> i32 {
    let transition_ref = match request.string_param(0) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let object_ref = match request.string_param(1) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let previous_state_ref = match request.string_param(2) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let next_state_ref = match request.string_param(3) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let balance_member_ref = match request.string_param(4) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let balance_state_ref = match request.string_param(5) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let register_member_ref = match request.string_param(6) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let register_state_ref = match request.string_param(7) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let proof_receipt_hash = match request.string_param(8) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let intent_id = match request.string_param(9) {
        Ok(value) => value,
        Err(code) => return code,
    };

    let descriptor = StateDescriptorInput {
        state_class: StateClass::ProgramObject,
        codec: "private_object_manifest_v1",
        schema_hash: None,
        subject_addr: None,
        hfhe_profile: HfheProfile::None,
        mutable_state: true,
    };
    let policy = StatePolicyInput {
        delivery_key_id: Some("room-key"),
        activate_after_epoch: 2,
        expire_after_epoch: 90,
    };
    if let Err(code) = Host::state_describe(next_state_ref, &descriptor) {
        return code;
    }
    if let Err(code) = Host::state_publish(next_state_ref, &policy) {
        return code;
    }
    let transition = ObjectTransitionInput {
        transition_ref,
        object_ref,
        previous_state_ref,
        next_state_ref,
        proof_kind: ProofKind::BoundZeroReceiptV1,
        proof_receipt_hash,
        status: "operational",
        intent_id,
    };
    let deltas = [
        ObjectMemberDelta::Attach {
            member_ref: balance_member_ref,
            input: ObjectMemberInput {
                state_ref: balance_state_ref,
                member_kind: "balance_role",
                state_class: StateClass::BalanceCell,
                codec: "balance_cell_v1",
                status: "live",
            },
        },
        ObjectMemberDelta::Attach {
            member_ref: register_member_ref,
            input: ObjectMemberInput {
                state_ref: register_state_ref,
                member_kind: "register_role",
                state_class: StateClass::RegisterCell,
                codec: "register_cell_v1",
                status: "live",
            },
        },
    ];

    match Host::object_transition_apply(&transition, &deltas) {
        Ok(version) => Host::respond_value(Value::Int(version.to_string())),
        Err(code) => code,
    }
}

fn run_query(ptr: i32, len: i32) -> i32 {
    let request = match decode_request(ptr, len) {
        Ok(request) => request,
        Err(code) => return code,
    };
    match request.method.as_str() {
        "get_counter" => match load_counter() {
            Ok(counter) => Host::respond_value(Value::Int(counter.to_string())),
            Err(code) => code,
        },
        "caller_echo" => match Host::caller() {
            Ok(caller) => Host::respond_value(Value::String(caller)),
            Err(code) => code,
        },
        "state_path_key_of" => match request.string_param(0).and_then(Host::state_path_key) {
            Ok(path_key) => Host::respond_value(Value::String(path_key)),
            Err(code) => code,
        },
        "runtime_guard" => Host::respond_value(Value::String(String::from("wasm_vm_only"))),
        "public_asset_read" => match Host::circle_public_asset_read(
            "oct11111111111111111111111111111111111111111111",
            "/public.bin",
            2,
            4,
        ) {
            Ok(body_b64) => Host::respond_value(Value::String(body_b64)),
            Err(code) => code,
        },
        "public_asset_undeclared" => match Host::circle_public_asset_read(
            "oct11111111111111111111111111111111111111111111",
            "/public.bin",
            2,
            4,
        ) {
            Ok(body_b64) => Host::respond_value(Value::String(body_b64)),
            Err(code) => code,
        },
        "state_class_of" => string_result(
            request
                .string_param(0)
                .and_then(|state_ref| Host::state_descriptor_get(state_ref, StateDescriptorField::StateClass))
        ),
        "state_delivery_key_of" => string_result(
            request
                .string_param(0)
                .and_then(|state_ref| Host::state_policy_get(state_ref, StatePolicyField::DeliveryKeyId))
        ),
        "balance_commitment_of" => string_result(
            request
                .string_param(0)
                .and_then(|state_ref| Host::balance_cell_get(state_ref, BalanceCellField::AmountCommitment))
        ),
        "balance_workflow_status_of" => string_result(
            request
                .string_param(0)
                .and_then(|workflow_ref| Host::balance_workflow_get(workflow_ref, BalanceWorkflowField::Status))
        ),
        "object_status_of" => string_result(
            request
                .string_param(0)
                .and_then(|object_ref| Host::object_binding_get(object_ref, ObjectBindingField::Status))
        ),
        "object_last_transition_of" => string_result(
            request
                .string_param(0)
                .and_then(|object_ref| Host::object_binding_get(object_ref, ObjectBindingField::LastTransitionRef))
        ),
        "object_member_status_of" => {
            let object_ref = match request.string_param(0) {
                Ok(value) => value,
                Err(code) => return code,
            };
            let member_ref = match request.string_param(1) {
                Ok(value) => value,
                Err(code) => return code,
            };
            string_result(Host::object_member_get(object_ref, member_ref, ObjectMemberField::Status))
        }
        "object_quorum_of" => string_result(
            request
                .string_param(0)
                .and_then(|object_ref| Host::object_policy_get(object_ref, ObjectPolicyField::MemberQuorum))
        ),
        "binding_version_of" => string_result(
            request
                .string_param(0)
                .and_then(|subject_addr| Host::balance_binding_get(subject_addr, BalanceBindingField::Version))
        ),
        "register_proof_kind_of" => string_result(
            request
                .string_param(0)
                .and_then(|state_ref| Host::register_cell_get(state_ref, RegisterCellField::ProofKind))
        ),
        "register_binding_version_of" => string_result(
            request
                .string_param(0)
                .and_then(|register_ref| Host::register_binding_get(register_ref, RegisterBindingField::Version))
        ),
        "register_workflow_status_of" => string_result(
            request
                .string_param(0)
                .and_then(|workflow_ref| Host::register_workflow_get(workflow_ref, RegisterWorkflowField::Status))
        ),
        "fhe_commit_loaded_pk_of" => {
            let requested_addr = match request.string_param(0) {
                Ok(value) => value,
                Err(code) => return code,
            };
            let ciphertext = match request.string_param(1) {
                Ok(value) => value,
                Err(code) => return code,
            };
            let pubkey_b64 = match Host::fhe_load_pk(requested_addr) {
                Ok(value) => value,
                Err(code) => return code,
            };
            match Host::fhe_commit(&pubkey_b64, ciphertext) {
                Ok(value) => Host::respond_value(Value::String(value)),
                Err(code) => code,
            }
        }
        "fhe_encrypt_of" => {
            let amount = match request.int_param(0) {
                Ok(value) => value,
                Err(code) => return code,
            };
            let seed_b64 = match request.string_param(1) {
                Ok(value) => value,
                Err(code) => return code,
            };
            match Host::fhe_encrypt(amount, seed_b64) {
                Ok(value) => Host::respond_value(Value::String(value)),
                Err(code) => code,
            }
        }
        "fhe_encrypt_zero_of" => {
            let seed_b64 = match request.string_param(0) {
                Ok(value) => value,
                Err(code) => return code,
            };
            match Host::fhe_encrypt_zero(seed_b64) {
                Ok(value) => Host::respond_value(Value::String(value)),
                Err(code) => code,
            }
        }
        "fhe_decrypt_of" => {
            let ciphertext = match request.string_param(0) {
                Ok(value) => value,
                Err(code) => return code,
            };
            match Host::fhe_decrypt(ciphertext) {
                Ok(value) => Host::respond_value(Value::Int(value.to_string())),
                Err(code) => code,
            }
        }
        "fhe_add_of" => {
            let pubkey_b64 = match request.string_param(0) {
                Ok(value) => value,
                Err(code) => return code,
            };
            let lhs_ciphertext = match request.string_param(1) {
                Ok(value) => value,
                Err(code) => return code,
            };
            let rhs_ciphertext = match request.string_param(2) {
                Ok(value) => value,
                Err(code) => return code,
            };
            match Host::fhe_add(pubkey_b64, lhs_ciphertext, rhs_ciphertext) {
                Ok(value) => Host::respond_value(Value::String(value)),
                Err(code) => code,
            }
        }
        "fhe_sub_of" => {
            let pubkey_b64 = match request.string_param(0) {
                Ok(value) => value,
                Err(code) => return code,
            };
            let lhs_ciphertext = match request.string_param(1) {
                Ok(value) => value,
                Err(code) => return code,
            };
            let rhs_ciphertext = match request.string_param(2) {
                Ok(value) => value,
                Err(code) => return code,
            };
            match Host::fhe_sub(pubkey_b64, lhs_ciphertext, rhs_ciphertext) {
                Ok(value) => Host::respond_value(Value::String(value)),
                Err(code) => code,
            }
        }
        "fhe_scale_of" => {
            let pubkey_b64 = match request.string_param(0) {
                Ok(value) => value,
                Err(code) => return code,
            };
            let ciphertext = match request.string_param(1) {
                Ok(value) => value,
                Err(code) => return code,
            };
            let factor = match request.int_param(2) {
                Ok(value) => value,
                Err(code) => return code,
            };
            match Host::fhe_scale(pubkey_b64, ciphertext, factor) {
                Ok(value) => Host::respond_value(Value::String(value)),
                Err(code) => code,
            }
        }
        "fhe_add_const_of" => {
            let pubkey_b64 = match request.string_param(0) {
                Ok(value) => value,
                Err(code) => return code,
            };
            let ciphertext = match request.string_param(1) {
                Ok(value) => value,
                Err(code) => return code,
            };
            let amount = match request.int_param(2) {
                Ok(value) => value,
                Err(code) => return code,
            };
            match Host::fhe_add_const(pubkey_b64, ciphertext, amount) {
                Ok(value) => Host::respond_value(Value::String(value)),
                Err(code) => code,
            }
        }
        "fhe_sub_const_of" => {
            let pubkey_b64 = match request.string_param(0) {
                Ok(value) => value,
                Err(code) => return code,
            };
            let ciphertext = match request.string_param(1) {
                Ok(value) => value,
                Err(code) => return code,
            };
            let amount = match request.int_param(2) {
                Ok(value) => value,
                Err(code) => return code,
            };
            match Host::fhe_sub_const(pubkey_b64, ciphertext, amount) {
                Ok(value) => Host::respond_value(Value::String(value)),
                Err(code) => code,
            }
        }
        "fhe_pedersen_of" => {
            let amount = match request.int_param(0) {
                Ok(value) => value,
                Err(code) => return code,
            };
            let blinding_b64 = match request.string_param(1) {
                Ok(value) => value,
                Err(code) => return code,
            };
            match Host::fhe_pedersen(amount, blinding_b64) {
                Ok(value) => Host::respond_value(Value::String(value)),
                Err(code) => code,
            }
        }
        "fhe_commit_of" => {
            let pubkey_b64 = match request.string_param(0) {
                Ok(value) => value,
                Err(code) => return code,
            };
            let ciphertext = match request.string_param(1) {
                Ok(value) => value,
                Err(code) => return code,
            };
            match Host::fhe_commit(pubkey_b64, ciphertext) {
                Ok(value) => Host::respond_value(Value::String(value)),
                Err(code) => code,
            }
        }
        "fhe_bound_commitment_of" => {
            let pubkey_b64 = match request.string_param(0) {
                Ok(value) => value,
                Err(code) => return code,
            };
            let ciphertext = match request.string_param(1) {
                Ok(value) => value,
                Err(code) => return code,
            };
            let amount = match request.int_param(2) {
                Ok(value) => value,
                Err(code) => return code,
            };
            match Host::fhe_bound_commitment(pubkey_b64, ciphertext, amount) {
                Ok(value) => Host::respond_value(Value::String(value)),
                Err(code) => code,
            }
        }
        "fhe_verify_zero_of" => {
            let pubkey_b64 = match request.string_param(0) {
                Ok(value) => value,
                Err(code) => return code,
            };
            let ciphertext = match request.string_param(1) {
                Ok(value) => value,
                Err(code) => return code,
            };
            let proof = match request.string_param(2) {
                Ok(value) => value,
                Err(code) => return code,
            };
            match Host::fhe_verify_zero(pubkey_b64, ciphertext, proof) {
                Ok(value) => Host::respond_value(Value::Bool(value)),
                Err(code) => code,
            }
        }
        "fhe_verify_range_of" => {
            let pubkey_b64 = match request.string_param(0) {
                Ok(value) => value,
                Err(code) => return code,
            };
            let ciphertext = match request.string_param(1) {
                Ok(value) => value,
                Err(code) => return code,
            };
            let proof = match request.string_param(2) {
                Ok(value) => value,
                Err(code) => return code,
            };
            let amount_commitment = match request.string_param(3) {
                Ok(value) => value,
                Err(code) => return code,
            };
            match Host::fhe_verify_range(
                pubkey_b64,
                ciphertext,
                proof,
                amount_commitment,
            ) {
                Ok(value) => Host::respond_value(Value::Bool(value)),
                Err(code) => code,
            }
        }
        "fhe_verify_bound_of" => {
            let pubkey_b64 = match request.string_param(0) {
                Ok(value) => value,
                Err(code) => return code,
            };
            let ciphertext = match request.string_param(1) {
                Ok(value) => value,
                Err(code) => return code,
            };
            let proof = match request.string_param(2) {
                Ok(value) => value,
                Err(code) => return code,
            };
            let amount_commitment = match request.string_param(3) {
                Ok(value) => value,
                Err(code) => return code,
            };
            match Host::fhe_verify_bound(pubkey_b64, ciphertext, proof, amount_commitment) {
                Ok(value) => Host::respond_value(Value::Bool(value)),
                Err(code) => code,
            }
        }
        _ => 61,
    }
}

fn run_update(ptr: i32, len: i32) -> i32 {
    let request = match decode_request(ptr, len) {
        Ok(request) => request,
        Err(code) => return code,
    };
    match request.method.as_str() {
        "inc" => {
            let next = match load_counter() {
                Ok(counter) => counter + 1,
                Err(code) => return code,
            };
            if let Err(code) = store_counter(next) {
                return code;
            }
            if let Ok(caller) = Host::caller() {
                let _ = Host::kv_put_string("last_caller", &caller);
            }
            let _ = Host::emit_event("circle.counter", next.to_string().as_bytes());
            Host::respond_value(Value::Int(next.to_string()))
        }
        "public_asset_record" => match Host::circle_public_asset_read(
            "oct11111111111111111111111111111111111111111111",
            "/public.bin",
            2,
            4,
        ) {
            Ok(body_b64) => match Host::kv_put_string("public_asset_record", &body_b64) {
                Ok(()) => Host::respond_value(Value::String(body_b64)),
                Err(code) => code,
            },
            Err(code) => code,
        },
        "prepare_room" => prepare_room(&request),
        "prepare_register_lane" => prepare_register_lane(&request),
        "promote_room" => promote_room(&request),
        "fhe_verify_zero_and_record" => {
            let pubkey_b64 = match request.string_param(0) {
                Ok(value) => value,
                Err(code) => return code,
            };
            let ciphertext = match request.string_param(1) {
                Ok(value) => value,
                Err(code) => return code,
            };
            let proof = match request.string_param(2) {
                Ok(value) => value,
                Err(code) => return code,
            };
            let verified = match Host::fhe_verify_zero(pubkey_b64, ciphertext, proof) {
                Ok(value) => value,
                Err(code) => return code,
            };
            if let Err(code) =
                Host::kv_put_string("last_hfhe_verification", verified.to_string().as_str())
            {
                return code;
            }
            Host::respond_value(Value::Bool(verified))
        }
        "kernel_probe" => {
            let _ = Host::kv_put_string("transport_policy:escape", "1");
            Host::respond_value(Value::Bool(true))
        }
        _ => 62,
    }
}

#[no_mangle]
pub extern "C" fn octra_manifest(_ptr: i32, _len: i32) -> i32 {
    respond_manifest()
}

#[no_mangle]
pub extern "C" fn octra_query(ptr: i32, len: i32) -> i32 {
    run_query(ptr, len)
}

#[no_mangle]
pub extern "C" fn octra_update(ptr: i32, len: i32) -> i32 {
    run_update(ptr, len)
}