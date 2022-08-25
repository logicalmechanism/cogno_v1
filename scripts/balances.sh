#!/usr/bin/bash
set -e

testnet_magic=$(cat data/testnet.magic)

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
script_path="../cogno-contract/cogno-contract.plutus"

script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic ${testnet_magic})

wallet_address=$(cat wallets/seller-wallet/payment.addr)
reference_address=$(cat wallets/reference-wallet/payment.addr)
collat_address=$(cat wallets/collat-wallet/payment.addr)

${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file tmp/protocol.json
${cli} query tip --testnet-magic ${testnet_magic} | jq

echo -e "\n\nScript Address:" ${script_address}
${cli} query utxo --address ${script_address} --testnet-magic ${testnet_magic}

echo -e "\n\nWallet Address:" ${wallet_address}
${cli} query utxo --address ${wallet_address} --testnet-magic ${testnet_magic}

echo -e "\n\nReference Address:" ${reference_address}
${cli} query utxo --address ${reference_address} --testnet-magic ${testnet_magic}

echo -e "\n\nCollat Address:" ${collat_address}
${cli} query utxo --address ${collat_address} --testnet-magic ${testnet_magic}