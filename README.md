## Octra Lite Node

This repository provides a complete, lightweight Node implementation for the Octra Network, offering full support for all runtime types, virtual machines, isolated execution environments, data storage, and distribution protocols. It also supports all core support and call-execution modules. You can combine nodes into your own private network — without participating in the main network — and subsequently configure synchronization with the main network to achieve maximum data isolation, should the need arise.

For the lite node, you might also consider using a Raspberry Pi, as shown in this post:
https://x.com/lambda0xE/status/2060325624426705227?s=20

Among the good providers for infra there are also Contabo and several others.

## Toolchain
- OCaml 4.14.2
- Dune 3.0 or newer (tested on 3.23.0)
- C++17 compiler
- Rust 1.80 or newer (tested on 1.80.1)
- GNU Make
- GMP, SQLite3 and libev development packages

`controls/install.sh --source-build` installs the system packages it needs.

## How to prepare the environment

```bash
sudo apt-get update
sudo apt-get install -y ca-certificates git
sudo install -d -m 0755 /opt/octra
sudo git clone --branch main --single-branch \
  https://github.com/octra-labs/lite_node.git \
  /opt/octra/libv_litecore

cd /opt/octra/libv_litecore
cat SOURCE_COMMIT

sudo env OCTRA_OPERATOR_USER=octra OCTRA_DATA_ROOT=/var/lib/octra \
  sh controls/install.sh --source-build

sudo -iu octra
cd /opt/octra/libv_litecore
```

## How to verify the package

```bash
sh controls/check.sh
```

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

## How to update (if you are already a validator)
> **MUST be taken into account before everything else:**
> Don't run `--sync` again, don't change `config/network.env`, and don't re-enroll the node. Consider the conditions of your config and your data paths as needed.

```bash
sudo -iu octra
cd /opt/octra/libv_litecore

git pull --ff-only
cat SOURCE_COMMIT
sh controls/check.sh

SWITCH="$PWD/runtime_data/toolchains/ocaml"

make -C mcl MCL_FP_BIT=256 MCL_FR_BIT=256 lib/libmcl.a

opam exec --switch "$SWITCH" -- dune build --profile release \
  bin/octra_node.exe \
  bin/octra_pvac_worker.exe \
  bin/octra_state_sync_client.exe \
  bin/octra_state_sync_manifest.exe \
  bin/bft_control_tx.exe

sh controls/stop.sh
sh controls/run.sh --rebind-runtime
sh controls/stat.sh
```
