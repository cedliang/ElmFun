module Types exposing (..)


import Json.Decode as Decode exposing (Decoder, int, string, list)
import Json.Decode.Pipeline exposing (required)




type alias PaymentAddress = 
    { bech32: String
    , cred: String
    }

type alias AssetOut = 
    { asset_name: String
    , policy_id: String
    , quantity: Int
    }

type alias TxIn = 
    { payment_addr: PaymentAddress
    , tx_hash: String
    , tx_index: Int
    , value: Int
    }

type alias TxOut = 
    { payment_addr: PaymentAddress
    , tx_index: Int
    , value: Int
    , asset_list: List AssetOut
    }

type alias Metadata =
    { json: String
    , key: Int
    }

type alias Tx =
    { tx_hash: String
    , block_height: Int
    , fee: Int
    , inputs: List TxIn
    , outputs: List TxOut
    , tx_block_index: Int
    -- , metadata: List Metadata
    }

type alias MultipleTxs = List Tx

paymentAddrDecoder : Decoder PaymentAddress
paymentAddrDecoder =
  Decode.succeed PaymentAddress
    |> required "bech32" string
    |> required "cred" string


assetOutDecoder : Decoder AssetOut
assetOutDecoder =
  Decode.succeed AssetOut
    |> required "asset_name" string
    |> required "policy_id" string
    |> required "quantity" int

txInDecoder : Decoder TxIn
txInDecoder =
  Decode.succeed TxIn
    |> required "payment_addr" paymentAddrDecoder
    |> required "tx_hash" string
    |> required "tx_index" int
    |> required "value" int

txOutDecoder : Decoder TxOut
txOutDecoder =
  Decode.succeed TxOut
    |> required "payment_addr" paymentAddrDecoder
    |> required "tx_index" int
    |> required "value" int
    |> required "asset_list" (list assetOutDecoder)

-- metadataDecoder : Decoder Metadata
-- metadataDecoder = 
--   Decode.succeed Metadata
--     |> required "json" string
--     |> required "key" int


txDecoder : Decoder Tx
txDecoder =
  Decode.succeed Tx
    |> required "tx_hash" string
    |> required "block_height" int
    |> required "fee" int
    |> required "inputs" (list txInDecoder)
    |> required "outputs" (list txOutDecoder)
    |> required "tx_block_index" int
    -- |> required "metadata" (list metadataDecoder)

multipleTxsDecoder : Decoder MultipleTxs
multipleTxsDecoder = 
  list txDecoder