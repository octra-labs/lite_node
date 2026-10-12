// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

use super::*;

const MAX_STEPS: usize = 64;
const MAX_INPUT: usize = 16 * 1024 * 1024;
const MAX_OUTPUT: usize = 4 * 1024 * 1024;
const MAX_FUEL: u64 = 10_000_000;

#[derive(Deserialize)]
struct Input {
    #[serde(deserialize_with = "read_steps")]
    replay_steps: Vec<String>,
}

fn read_steps<'de, D: serde::Deserializer<'de>>(reader: D) -> Result<Vec<String>, D::Error> {
    struct Steps;
    impl<'de> serde::de::Visitor<'de> for Steps {
        type Value = Vec<String>;

        fn expecting(&self, output: &mut std::fmt::Formatter) -> std::fmt::Result {
            output.write_str("a finite list of compute inputs")
        }

        fn visit_seq<A: serde::de::SeqAccess<'de>>(self, mut input: A)
            -> Result<Self::Value, A::Error>
        {
            let mut values = Vec::with_capacity(MAX_STEPS);
            for _ in 0..MAX_STEPS {
                match input.next_element()? {
                    Some(value) => values.push(value),
                    None => return Ok(values),
                }
            }
            if input.next_element::<serde::de::IgnoredAny>()?.is_some() {
                return Err(serde::de::Error::custom("compute replay step count"));
            }
            Ok(values)
        }
    }
    reader.deserialize_seq(Steps)
}

pub(super) fn run(raw: &str) -> Result<JsonValue, HostFailure> {
    if raw.len() > MAX_INPUT * 2 {
        return Err(HostFailure::Rejected("compute replay wire size".to_owned()));
    }
    let _input = compute_memory().take(raw.len().saturating_mul(2))
        .map_err(|reason| HostFailure::Unavailable(reason.to_owned()))?;
    let input: Input = serde_json::from_str(raw)
        .map_err(|_| HostFailure::Rejected("compute replay input refused".to_owned()))?;
    let steps = &input.replay_steps;
    if steps.is_empty() || steps.len() > MAX_STEPS {
        return Err(HostFailure::Rejected("compute replay step count".to_owned()));
    }
    let size = steps.iter().try_fold(0usize, |size, raw| size.checked_add(raw.len()))
        .filter(|size| *size <= MAX_INPUT)
        .ok_or_else(|| HostFailure::Rejected("compute replay input size".to_owned()))?;
    let _memory = compute_memory().take(size.saturating_mul(2).saturating_add(MAX_OUTPUT * 4))
        .map_err(|reason| HostFailure::Unavailable(reason.to_owned()))?;
    let parsed: Vec<Payload> = steps.iter().map(|raw| serde_json::from_str(raw)
        .map_err(|_| HostFailure::Rejected("compute replay input refused".to_owned())))
        .collect::<Result<_, _>>()?;
    let first = &parsed[0];
    if first.code_b64.as_deref().is_none_or(str::is_empty) {
        return Err(HostFailure::Rejected("compute replay code missing".to_owned()));
    }
    if first.caller.as_deref().unwrap_or("").is_empty()
        || first.address.as_deref().unwrap_or("").is_empty()
    {
        return Err(HostFailure::Rejected("compute replay owner missing".to_owned()));
    }
    let mut fuel = 0u64;
    for step in &parsed {
        if step.action.as_deref() != Some("execute") || step.is_view != Some(true)
            || step.execution_profile.as_deref() != Some("compute")
            || step.compute_session_scope.is_none()
            || step.compute_session_scope != first.compute_session_scope
            || step.caller != first.caller || step.address != first.address
            || step.code_cache_key != first.code_cache_key
            || step.code_b64.as_deref() != first.code_b64.as_deref()
            || step.compute_storage_cache_key != first.compute_storage_cache_key
            || step.compute_storage_root != first.compute_storage_root
            || step.compute_storage_entries != first.compute_storage_entries
            || step.compute_storage_bytes != first.compute_storage_bytes
            || step.storage_cache_key.is_some()
        {
            return Err(HostFailure::Rejected("compute replay scope differs".to_owned()));
        }
        fuel = fuel.checked_add(step.fuel_limit.unwrap_or(COMPUTE_DEFAULT_FUEL_LIMIT))
            .filter(|fuel| *fuel <= MAX_FUEL)
            .ok_or_else(|| HostFailure::Rejected("compute replay fuel limit".to_owned()))?;
    }
    let _model = match first.compute_storage_cache_key.as_ref() {
        None => None,
        Some(key) => {
            let root = first.compute_storage_root.as_ref()
                .ok_or_else(|| HostFailure::Rejected("compute replay model root missing".to_owned()))?;
            let entries = first.compute_storage_entries
                .ok_or_else(|| HostFailure::Rejected("compute replay model entries missing".to_owned()))?;
            let bytes = first.compute_storage_bytes
                .ok_or_else(|| HostFailure::Rejected("compute replay model bytes missing".to_owned()))?;
            match compute_cache_call(CacheMessage::Hold {
                key: key.clone(), root: root.clone(), entries, bytes,
            }).map_err(HostFailure::Unavailable)? {
                CacheReply::Held(id) => Some(ComputeLease { id: Some(id) }),
                _ => return Err(HostFailure::Unavailable("compute replay model unavailable".to_owned())),
            }
        }
    };
    let owner = Arc::new(SessionOwner::default());
    let mut replies = Vec::with_capacity(parsed.len());
    let mut output_size = 0usize;
    for (index, step) in parsed.iter().enumerate() {
        let (mut runtime, request, export) = prepare_owned_execution(step, Some(owner.clone()))?;
        let call = finish_call(&mut runtime, export, &request).map_err(HostFailure::Rejected)?;
        if let Some(error) = runtime.store.data().unavailable.as_ref() {
            return Err(HostFailure::Unavailable(error.clone()));
        }
        if call.code != 0 && index + 1 != parsed.len() {
            return Err(HostFailure::Rejected("compute replay prior call stopped".to_owned()));
        }
        let raw = reply::execute(&runtime, &call)?;
        output_size = output_size.checked_add(raw.len())
            .filter(|size| *size <= MAX_OUTPUT)
            .ok_or_else(|| HostFailure::Rejected("compute replay output size".to_owned()))?;
        replies.push(serde_json::from_slice::<JsonValue>(&raw)
            .map_err(|_| HostFailure::Unavailable("compute replay reply refused".to_owned()))?);
    }
    Ok(json!({"replies": replies}))
}