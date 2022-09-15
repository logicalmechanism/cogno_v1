#!/bin/bash
set -e

# Paths
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
script_path="../cogno-contract/cogno-contract.plutus"
TESTNET_MAGIC=$(cat data/testnet.magic)

# Addresses
script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic ${TESTNET_MAGIC})
issuer_address=$(cat wallets/seller-wallet/payment.addr)
issuer_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/seller-wallet/payment.vkey)

reference_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/reference-wallet/payment.vkey)

lock_value=10000000

sc_address_out="${script_address} + ${lock_value}"
echo "Script OUTPUT: "${sc_address_out}
#
# exit
#
echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${TESTNET_MAGIC} \
    --address ${issuer_address} \
    --out-file tmp/issuer_utxo.json

TXNS=$(jq length tmp/issuer_utxo.json)
if [ "$TXNS" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${vestor_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/issuer_utxo.json)
CTXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in-collateral"' tmp/issuer_utxo.json)
collateral_tx_in=${CTXIN::-19}
issuer_tx_in=${TXIN::-8}

echo -e "\033[0;36m Gathering Script UTxO Information  \033[0m"
${cli} query utxo \
    --address ${script_address} \
    --testnet-magic ${TESTNET_MAGIC} \
    --out-file tmp/script_utxo.json
# transaction variables
TXNS=$(jq length tmp/script_utxo.json)
if [ "$TXNS" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${script_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/script_utxo.json)
script_tx_in=${TXIN::-8}

script_ref_utxo=$(${cli} transaction txid --tx-file tmp/tx-reference-utxo.signed)
tag_utxo=$(${cli} transaction txid --tx-file tmp/royalty-tx.signed)

# collat info
collat_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/collat-wallet/payment.vkey)
collat_utxo="10e5b05d90199da3f7cb581f00926f5003e22aac8a3d5a33607cd4c57d13aaf3" # in collat wallet

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${issuer_address} \
    --tx-in ${issuer_tx_in} \
    --tx-in-collateral="${collat_utxo}#0" \
    --tx-in="${tag_utxo}#1" \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file data/redeemer/update_redeemer.json \
    --tx-out="${sc_address_out}" \
    --tx-out-inline-datum-file data/datum/royalty_datum.json \
    --required-signer-hash ${issuer_pkh} \
    --required-signer-hash ${collat_pkh} \
    --testnet-magic ${TESTNET_MAGIC})

IFS=':' read -ra VALUE <<< "$FEE"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
#
# exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file wallets/seller-wallet/payment.skey \
    --signing-key-file wallets/collat-wallet/payment.skey \
    --tx-body-file tmp/tx.draft \
    --out-file tmp/royalty-tx.signed \
    --testnet-magic ${TESTNET_MAGIC}
#
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-file tmp/royalty-tx.signed
