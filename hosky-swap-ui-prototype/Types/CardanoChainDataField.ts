import CardanoAsset from "./CardanoAsset";

type CardanoChainDataField = {
    Id: string,
    CreatedAt: Date,
    Type: number,
    Key: string,
    Value: string | number
}

export default CardanoChainDataField;