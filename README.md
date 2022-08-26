# cogno

```
cognomen 

Noun

A word or set of words by which a person or thing is known, addressed, or referred to

A familiar, invented given name for a person or thing 

used instead of the actual name of the person or thing
```

A smart contract for UTxO based wallet cognomens. Each UTxO is a wallet identifier similar to ada handles but instead of NFTs representing the nickname it will be a UTxO inside a smart contract.

## Use

A user is always in control of their cognomen. If the public key hash is present upon removal then the user may do as they wish with their Cogno deposit. A user can update their Cogno data at any time. The ownership of the UTxO must remain the same but the utxo owner may change any other data in the datum.

The primary use case of this contract is for reference. This contract will contain information with known wallet information.

## The basic Cogno

A wallet will create a Cogno by supplying a payment public key hash, a staking credential, and a cognomen.


```hs
data CognoData = CognoData
  { cdPkh   :: PlutusV2.PubKeyHash
  -- ^ The public key hash of the wallet.
  , cdSc    :: PlutusV2.PubKeyHash
  -- ^ The stake hash of the wallet.
  , cdCogno :: PlutusV2.BuiltinByteString
  -- ^ The cognomen of the wallet.
  ...
  }
```

This is all the information required to identify a wallet address with a cognomen.

### Example

Assume the wallet address is

```
addr1qxvlcxj3fg3jk2dp3kmkxhnx2zuv7edktk5rfy2n9juj3h2m0cw9csycjc4v59ywy7fk8nqfu6qjdzjejhvayfhf8dwsttnjt6
```

then the hash representation is

```
0199fc1a514a232b29a18db7635e6650b8cf65b65da83491532cb928dd5b7e1c5c4098962aca148e279363cc09e681268a5995d9d226e93b5d
```

For this example address, the datum will become

```json
{
  "constructor": 0,
  "fields": [
    {
      "constructor": 0,
      "fields": [
        {
          "bytes": "99fc1a514a232b29a18db7635e6650b8cf65b65da83491532cb928dd"
        },
        {
          "bytes": "5b7e1c5c4098962aca148e279363cc09e681268a5995d9d226e93b5d"
        },
        {
          "int": 0
        },
        {
          "bytes": "546865416e6369656e744b72616b656e"
        },
        {
          "list": []
        },
        {
          "list": []
        },
        {
          "bytes": ""
        }
      ]
    }
  ]
}
```
The other information contained in the datum are for holding information to create a custom profile for the cogno.

## Use Case

When a wallet address is queried, the wallet address can be cross reference with datum data from this contract to relay information about that specific wallet. This behaves very similarly to already existing NFT based identification but the key difference is the updatable data and that it can be referenced on-chain. Smart contracts will now have the ability to reference a wallets Cogno and use that data in on-chain validation functions.

# tag

TODO