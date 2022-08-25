cabal clean
cabal update
cabal build -w ghc-8.10.7 -O2
cabal run cogno-contract
cardano-cli address build --payment-script-file cogno-contract.plutus --testnet-magic 1097911063 --out-file validator.addr
echo -e "\nValidator Testnet Address:" $(cat validator.addr)
cardano-cli transaction policyid --script-file cogno-contract.plutus > validator.hash
echo -e "\nValidator Hash:" $(cat validator.hash)
python3 -c "import binascii;a='$(cat validator.hash)';s=binascii.unhexlify(a);print([x for x in s])" > validator.bytes
echo -e "\nValidator Bytes:" $(cat validator.bytes)
echo "DONE"