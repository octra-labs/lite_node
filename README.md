## Octra Lite Node

This repository provides a complete, lightweight Node implementation for the Octra Network, offering full support for all runtime types, virtual machines, isolated execution environments, data storage, and distribution protocols. It also supports all core support and call-execution modules. You can combine nodes into your own private network — without participating in the main network — and subsequently configure synchronization with the main network to achieve maximum data isolation, should the need arise.
Please note that this package will be updated over time to establish a canonical version, taking into account factors such as new node connections, load handling, and test results.

For the light node, you might also consider using a Raspberry Pi, as shown in this post:
https://x.com/lambda0xE/status/2060325624426705227?s=20

## Toolchain
- OCaml 4.14.2
- Dune 3.0 or newer
- C++17 compiler
- Rust 1.80 or newer
- GNU Make
- GMP, SQLite3 and libev development packages

## How to prepare the environment

```bash
sudo env OCTRA_OPERATOR_USER=octra OCTRA_DATA_ROOT=/var/lib/octra \
  sh controls/install.sh --source-build
sudo -iu octra
cd /opt/octra/libv_litecore
```

## How to build

```bash
opam install . --deps-only --with-test --locked
make -C mcl MCL_FP_BIT=256 MCL_FR_BIT=256 lib/libmcl.a
opam exec -- dune build --root . --profile release \
  bin/octra_node.exe \
  bin/octra_pvac_worker.exe \
  bin/octra_state_sync_client.exe \
  bin/octra_state_sync_manifest.exe \
  bin/bft_control_tx.exe
```

## How to verify the package

```bash
sh controls/check.sh
```

## How to configure an observer
The network bundle `config/` ships with the operator archive.

```bash
printf 'Public DNS name or IP: '
read -r PUBLIC_HOST
printf 'Node name: '
read -r NODE_NAME
sha256sum -c config/network.env.sha256
NETWORK_SHA256=$(awk '{print $1}' config/network.env.sha256)
sh controls/config_val.sh \
  --role observer \
  --name "$NODE_NAME" \
  --advertise "$PUBLIC_HOST:19000" \
  --api-port 8080 \
  --consensus-port 19000 \
  --p2p-port 9000 \
  --data-dir /var/lib/octra/devnet \
  --sync-stage /var/lib/octra/devnet.state_sync \
  --network config/network.env \
  --network-sha "$NETWORK_SHA256" \
  --build \
  --sync \
  --yes
```

## How to start and check things

```bash
sh controls/run.sh
sh controls/stat.sh
sh controls/stat.sh --logs
```

## How to join the net

```bash
sh controls/enroll.sh join --amount 1000000
sh controls/enroll.sh status
sh controls/stat.sh
```

## How to leave the active set

```bash
sh controls/enroll.sh exit
sh controls/enroll.sh status
sh controls/enroll.sh withdraw
```

## How to stop the node

```bash
sh controls/stop.sh
```