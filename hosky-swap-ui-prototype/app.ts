import { TransactionUnspentOutput, BigNum, Vkeywitnesses, ScriptHash, AssetName, PlutusData, Ed25519KeyHash, BaseAddress, Redeemer, PlutusScripts } from '@emurgo/cardano-serialization-lib-browser';
import CardanoLoader from './CardanoLoader';
import CardanoProtocolParameters from './Types/CardanoProtocolParam';
import { contractCbor } from "./contract";

const BLOCKFROST_PROJECT_ID = "testnetIQtFV2rODiSJP0ROecSNx89tPRQ69mfN";
const btnSelector = "btnBuy";
const CardanoSerializationLib = CardanoLoader.CardanoSerializationLib;

async function Main() {
    await CardanoLoader.LoadAsync();
    const btnContract = document.getElementById(btnSelector) as HTMLButtonElement;
    btnContract.addEventListener("click", ExecuteContract);
}

async function ExecuteContract() {
    try {
        // await BuildOfferTxAsync();
        await BuildSwapTxAsync();
    } catch (e) {
        console.dir(e);
    }
}

async function BuildOfferTxAsync() {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        if (!await window.cardano.isEnabled()) await window.cardano.enable();
        const transactionWitnessSet = Cardano.TransactionWitnessSet.new();
        const transactionInputs = Cardano.TransactionInputs.new();
        const transactionOutputs = Cardano.TransactionOutputs.new();
        const datumHash = Cardano.hash_plutus_data(HoskySwapDatum() as PlutusData);
        console.log(toHex(datumHash.to_bytes()));
        transactionInputs.add(
            Cardano.TransactionInput.new(
                Cardano.TransactionHash.from_bytes(fromHex("2ea40cfee92e08c0aa9ec17a3295fc38e756007897ad04eca91cba031d0f2820")), 0
            )
        );

        transactionInputs.add(
            Cardano.TransactionInput.new(
                Cardano.TransactionHash.from_bytes(fromHex("d3a22ab789a3d7e00e9a8ffa70c92e42f83463e98261ab067b5d398e2a0dcfd7")), 0
            )
        );

        const contractOutput = Cardano.TransactionOutput.new(
            Cardano.Address.from_bech32("addr_test1wqqtxf09mr74k8axgxskql5rcyfwgqyth34jvk0ksccucdg8t8v3q"),
            Cardano.Value.new(toBigNum("10000000"))
        );
        contractOutput.set_data_hash(datumHash);

        transactionOutputs.add(contractOutput);

        transactionOutputs.add(
            Cardano.TransactionOutput.new(
                Cardano.Address.from_bech32("addr_test1qqly5tk8pl80ne2vgdapwdc568uzh93yyducz6l28hfc0htkdqgkydtswrycyf9hruerftc8mwel9ck6pksvyszs968qhauh2s"),
                Cardano.Value.new(toBigNum("9500000"))
            )
        );

        const transactionBody = Cardano.TransactionBody.new(
            transactionInputs,
            transactionOutputs,
            toBigNum("500000")
        );

        const transaction = Cardano.Transaction.new(
            Cardano.TransactionBody.from_bytes(transactionBody.to_bytes()),
            Cardano.TransactionWitnessSet.from_bytes(
                transactionWitnessSet.to_bytes()
            )
        );

        const serializedTx = toHex(transaction.to_bytes());

        const txVkeyWitnesses = await window.cardano.signTx(serializedTx, true);

        let signedtxVkeyWitnesses = Cardano.TransactionWitnessSet.from_bytes(
            fromHex(txVkeyWitnesses)
        );

        transactionWitnessSet.set_vkeys(signedtxVkeyWitnesses.vkeys() as Vkeywitnesses);

        const signedTx = Cardano.Transaction.new(
            Cardano.TransactionBody.from_bytes(transactionBody.to_bytes()),
            Cardano.TransactionWitnessSet.from_bytes(
                transactionWitnessSet.to_bytes()
            )
        );

        console.log("Full Tx Size", signedTx.to_bytes().length);
        let result = await window.cardano.submitTx(toHex(signedTx.to_bytes()));
        console.log("tx submitted", result);
    }
}

async function BuildSwapTxAsync() {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        if (!await window.cardano.isEnabled()) await window.cardano.enable();
        const transactionWitnessSet = Cardano.TransactionWitnessSet.new();
        const transactionInputs = Cardano.TransactionInputs.new();
        const transactionOutputs = Cardano.TransactionOutputs.new();

        const baseAddress = Cardano.BaseAddress.from_address(
            Cardano.Address.from_bech32("addr_test1qqly5tk8pl80ne2vgdapwdc568uzh93yyducz6l28hfc0htkdqgkydtswrycyf9hruerftc8mwel9ck6pksvyszs968qhauh2s")
        ) as BaseAddress;

        const requiredSigners = Cardano.Ed25519KeyHashes.new();
        requiredSigners.add(baseAddress.payment_cred().to_keyhash() as Ed25519KeyHash);

        const plutusDatum = HoskySwapDatum() as PlutusData;
        const datumList = Cardano.PlutusList.new();
        datumList.add(plutusDatum);

        const redeemers = Cardano.Redeemers.new();
        redeemers.add(SimpleRedeemer() as Redeemer);

        transactionWitnessSet.set_plutus_scripts(ContractScript() as PlutusScripts);
        transactionWitnessSet.set_plutus_data(Cardano.PlutusList.from_bytes(datumList.to_bytes()));
        transactionWitnessSet.set_redeemers(Cardano.Redeemers.from_bytes(redeemers.to_bytes()));

        transactionInputs.add(
            Cardano.TransactionInput.new(
                Cardano.TransactionHash.from_bytes(fromHex("ca240a8fa7ef1a86662b1ad0064566f76808748cd2c1a83a4041beac1c5a4efc")), 0
            )
        );

        transactionInputs.add(
            Cardano.TransactionInput.new(
                Cardano.TransactionHash.from_bytes(fromHex("63461348d5b18a4f05d28abaf4930d160912d5ac99433f4487d67c704f5b8fdf")), 0
            )
        );

        transactionOutputs.add(
            Cardano.TransactionOutput.new(
                Cardano.Address.from_bech32("addr_test1qqly5tk8pl80ne2vgdapwdc568uzh93yyducz6l28hfc0htkdqgkydtswrycyf9hruerftc8mwel9ck6pksvyszs968qhauh2s"),
                Cardano.Value.new(toBigNum("9500000"))
            )
        );

        const hoskyValue = Cardano.Value.new(toBigNum("1344798"));
        const hoskyMA = Cardano.MultiAsset.new();
        const hoskyAssets = Cardano.Assets.new();

        hoskyAssets.insert(
            Cardano.AssetName.new(fromHex("484f534b59")),
            toBigNum("10000000")
        );

        hoskyMA.insert(
            Cardano.ScriptHash.from_bytes(fromHex("88672eaaf6f5c5fb59ffa5b978016207dbbf769014c6870d31adc4de")),
            hoskyAssets
        );

        hoskyValue.set_multiasset(hoskyMA)

        transactionOutputs.add(
            Cardano.TransactionOutput.new(
                Cardano.Address.from_bech32("addr_test1qqly5tk8pl80ne2vgdapwdc568uzh93yyducz6l28hfc0htkdqgkydtswrycyf9hruerftc8mwel9ck6pksvyszs968qhauh2s"),
                hoskyValue
            )
        );

        const transactionBody = Cardano.TransactionBody.new(
            transactionInputs,
            transactionOutputs,
            toBigNum("500000")
        );

        transactionBody.set_required_signers(requiredSigners);

        const collateralUnspentTransactions = (await GetCollateralUnspentTransactionOutputAsync()) as TransactionUnspentOutput[];
        const collateralInputs = Cardano.TransactionInputs.new();
        collateralUnspentTransactions.forEach(c => collateralInputs.add(c.input()));
        transactionBody.set_collateral(collateralInputs);

        const auxData = Cardano.AuxiliaryData.new();
        auxData.set_plutus_scripts(ContractScript() as PlutusScripts);
        const transaction = Cardano.Transaction.new(
            Cardano.TransactionBody.from_bytes(transactionBody.to_bytes()),
            Cardano.TransactionWitnessSet.from_bytes(
                transactionWitnessSet.to_bytes()
            ),
            auxData
        );

        const serializedTx = toHex(transaction.to_bytes());

        const txVkeyWitnesses = await window.cardano.signTx(serializedTx, true);

        let signedtxVkeyWitnesses = Cardano.TransactionWitnessSet.from_bytes(
            fromHex(txVkeyWitnesses)
        );

        transactionWitnessSet.set_vkeys(signedtxVkeyWitnesses.vkeys() as Vkeywitnesses);

        const signedTx = Cardano.Transaction.new(
            Cardano.TransactionBody.from_bytes(transactionBody.to_bytes()),
            Cardano.TransactionWitnessSet.from_bytes(
                transactionWitnessSet.to_bytes()
            ),
            auxData
        );

        console.log("Full Tx Size", signedTx.to_bytes().length);
        let result = await window.cardano.submitTx(toHex(signedTx.to_bytes()));
        console.log("tx submitted", result);
    }
}

async function GetProtocolProtocolParamsAsync(): Promise<CardanoProtocolParameters> {
    let protocolParamsResult = await fetch("https://cardano-testnet.blockfrost.io/api/v0/epochs/latest/parameters", {
        "headers": {
            "project_id": BLOCKFROST_PROJECT_ID
        }
    });
    return await protocolParamsResult.json();
}

const HoskySwapDatum = () => {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        const datumFields = Cardano.PlutusList.new();
        datumFields.add(Cardano.PlutusData.new_integer(Cardano.BigInt.from_str("1000000")));
        datumFields.add(AssetClassDatum("", "") as PlutusData);
        datumFields.add(AssetClassDatum("88672eaaf6f5c5fb59ffa5b978016207dbbf769014c6870d31adc4de", "484f534b59") as PlutusData);
        datumFields.add(Cardano.PlutusData.new_bytes(fromHex("945c010ec1c1f884ed778c28b3c644dcc2da1c3b1df4a90924cc51de")));

        return Cardano.PlutusData.new_constr_plutus_data(
            Cardano.ConstrPlutusData.new(
                toBigNum("0"),
                datumFields
            )
        );
    }
    return null;
}

const SimpleRedeemer = () => {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        const redeemerData = Cardano.PlutusData.new_constr_plutus_data(
            Cardano.ConstrPlutusData.new(
                toBigNum("0"),
                Cardano.PlutusList.new()
            )
        );

        return Cardano.Redeemer.new(
            Cardano.RedeemerTag.new_spend(),
            Cardano.BigNum.from_str("0"),
            redeemerData,
            Cardano.ExUnits.new(
                Cardano.BigNum.from_str("4516627"),
                Cardano.BigNum.from_str("1712786295")
            )
        )
    }
    return null;
}

const AssetClassDatum = (policyId: string, assetName: string) => {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        const datumFields = Cardano.PlutusList.new();
        datumFields.add(Cardano.PlutusData.new_bytes(fromHex(policyId)));
        datumFields.add(Cardano.PlutusData.new_bytes(fromHex(assetName)));

        return Cardano.PlutusData.new_constr_plutus_data(
            Cardano.ConstrPlutusData.new(
                toBigNum("0"),
                datumFields
            )
        );
    }
}

const ContractScript = () => {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        const scripts = Cardano.PlutusScripts.new();
        scripts.add(Cardano.PlutusScript.new(fromHex(contractCbor)));
        return scripts;
    }
};

const GetCollateralUnspentTransactionOutputAsync = async () => {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        const utxosHex = await window.cardano.getCollateral();
        let utxos = utxosHex.map((utxoHex: string) => Cardano?.TransactionUnspentOutput.from_bytes(fromHex(utxoHex) ?? null));
        utxos = utxos.filter((utxo) => utxo !== undefined);
        return utxos as TransactionUnspentOutput[];
    }
};

const GetWalletAddressAsync = async () => (await window.cardano.getUsedAddresses())[0];
const toHex = (bytes: Uint8Array) => Buffer.from(bytes).toString("hex");
const fromHex = (hex: string) => Buffer.from(hex, "hex");

const toBigNum = (value: any) => {
    let Cardano = CardanoSerializationLib();
    return Cardano?.BigNum.from_str(value.toString()) as BigNum;
}

document.onreadystatechange = async () => {
    if (document.readyState == "complete") await Main();
};