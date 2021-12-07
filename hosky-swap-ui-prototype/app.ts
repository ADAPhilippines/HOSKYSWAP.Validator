import { TransactionUnspentOutput, BigNum, Vkeywitnesses, ScriptHash, AssetName, PlutusData, Ed25519KeyHash, BaseAddress, Redeemer, PlutusScripts, TransactionBuilder } from './custom_modules/@emurgo/cardano-serialization-lib-browser';
import CardanoLoader from './CardanoLoader';
import CardanoProtocolParameters from './Types/CardanoProtocolParam';
import { contractCbor } from "./contract";
import { languageViews } from './languageViews';
import { Value } from './custom_modules/@emurgo/cardano-serialization-lib-nodejs/cardano_serialization_lib';

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
        await BuildOfferTxAsync();
        // await BuildSwapTxAsync();
    } catch (e) {
        console.dir(e);
    }
}

async function BuildOfferTxAsync() {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        if (!await window.cardano.isEnabled()) await window.cardano.enable();

        const txBuilder = await CreateTransactionBuilderAsync() as TransactionBuilder;
        const selfAddress = Cardano.Address.from_bech32("addr_test1qqly5tk8pl80ne2vgdapwdc568uzh93yyducz6l28hfc0htkdqgkydtswrycyf9hruerftc8mwel9ck6pksvyszs968qhauh2s");
        const baseAddress = Cardano.BaseAddress.from_address(selfAddress) as BaseAddress;
        const pkh = toHex(baseAddress.payment_cred().to_keyhash()?.to_bytes() as Uint8Array);

        const transactionWitnessSet = Cardano.TransactionWitnessSet.new();
        const datumHash = Cardano.hash_plutus_data(HoskySwapDatum(pkh) as PlutusData);

        console.log("datumHash", toHex(datumHash.to_bytes()));
        console.log("pkh", pkh);

        txBuilder.add_input(
            selfAddress,
            Cardano.TransactionInput.new(
                Cardano.TransactionHash.from_bytes(fromHex("ca49c8f1f95f8c5e0901feb70a07645f15858a6965a2620ae532c8cf7d8786ec")), 1
            ),
            Cardano.Value.new(toBigNum("14830099"))
        );

        const contractOutput = Cardano.TransactionOutput.new(
            Cardano.Address.from_bech32("addr_test1wqqtxf09mr74k8axgxskql5rcyfwgqyth34jvk0ksccucdg8t8v3q"),
            Cardano.Value.new(toBigNum("10000000"))
        );
        contractOutput.set_data_hash(datumHash);

        txBuilder.add_output(contractOutput);

        txBuilder.add_change_if_needed(Cardano.Address.from_bech32("addr_test1qqly5tk8pl80ne2vgdapwdc568uzh93yyducz6l28hfc0htkdqgkydtswrycyf9hruerftc8mwel9ck6pksvyszs968qhauh2s"));

        const txBody = txBuilder.build();

        const transaction = Cardano.Transaction.new(
            Cardano.TransactionBody.from_bytes(txBody.to_bytes()),
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
            Cardano.TransactionBody.from_bytes(txBody.to_bytes()),
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
        const txBuilder = await CreateTransactionBuilderAsync() as TransactionBuilder;
        const transactionWitnessSet = Cardano.TransactionWitnessSet.new();
        
        const selfAddress = Cardano.Address.from_bytes(fromHex(await GetWalletAddressAsync()));
        const baseAddress = Cardano.BaseAddress.from_address(selfAddress) as BaseAddress;
        const pkh = "3e4a2ec70fcef9e54c437a173714d1f82b96242379816bea3dd387dd";
        const scriptInput =  Cardano.TransactionInput.new(
            Cardano.TransactionHash.from_bytes(fromHex("e2cf543405a6e479e97216a80df6bafc0bd47fed81e04e830ec82875acb07c05")), 0
        );

        txBuilder.add_input(
            Cardano.Address.from_bech32("addr_test1wqqtxf09mr74k8axgxskql5rcyfwgqyth34jvk0ksccucdg8t8v3q"),
            scriptInput,
            Cardano.Value.new(toBigNum("10000000"))
        );

        txBuilder.add_input(
            selfAddress,
            Cardano.TransactionInput.new(
                Cardano.TransactionHash.from_bytes(fromHex("ba5ba7e6bc259418fe16f8d41216f52ea64e9ca6b655d9202f9f7d08fba146fa")), 0
            ),
            AssetValue(
                toBigNum("5000000"),
                "88672eaaf6f5c5fb59ffa5b978016207dbbf769014c6870d31adc4de",
                "484f534b59",
                toBigNum("10000000")
            ) as Value
        );

        const scriptInputIndex = txBuilder.index_of_input(scriptInput);

        txBuilder.add_output(Cardano.TransactionOutput.new(
            Cardano.Address.from_bech32("addr_test1qqly5tk8pl80ne2vgdapwdc568uzh93yyducz6l28hfc0htkdqgkydtswrycyf9hruerftc8mwel9ck6pksvyszs968qhauh2s"),
            AssetValue(
                toBigNum("1500000"),
                "88672eaaf6f5c5fb59ffa5b978016207dbbf769014c6870d31adc4de",
                "484f534b59",
                toBigNum("10000000")
            ) as Value
        ));

        const requiredSigners = Cardano.Ed25519KeyHashes.new();
        requiredSigners.add(baseAddress.payment_cred().to_keyhash() as Ed25519KeyHash);
        txBuilder.set_required_signers(requiredSigners);

        const datum = HoskySwapDatum(pkh) as PlutusData;
        const datumList = Cardano.PlutusList.new();
        datumList.add(datum);

        const redeemers = Cardano.Redeemers.new();
        redeemers.add(SimpleRedeemer(scriptInputIndex) as Redeemer);

        txBuilder.set_plutus_scripts(ContractScript() as PlutusScripts);
        txBuilder.set_plutus_data(Cardano.PlutusList.from_bytes(datumList.to_bytes()));
        txBuilder.set_redeemers(Cardano.Redeemers.from_bytes(redeemers.to_bytes()));

        transactionWitnessSet.set_plutus_scripts(ContractScript() as PlutusScripts);
        transactionWitnessSet.set_plutus_data(Cardano.PlutusList.from_bytes(datumList.to_bytes()));
        transactionWitnessSet.set_redeemers(Cardano.Redeemers.from_bytes(redeemers.to_bytes()));

        const collateralUnspentTransactions = (await GetCollateralUnspentTransactionOutputAsync()) as TransactionUnspentOutput[];
        const collateralInputs = Cardano.TransactionInputs.new();
        collateralUnspentTransactions.forEach(c => collateralInputs.add(c.input()));
        txBuilder.set_collateral(collateralInputs);
        txBuilder.add_change_if_needed(selfAddress);

        const txBody = txBuilder.build();

        const transaction = Cardano.Transaction.new(
            Cardano.TransactionBody.from_bytes(txBody.to_bytes()),
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
            Cardano.TransactionBody.from_bytes(txBody.to_bytes()),
            Cardano.TransactionWitnessSet.from_bytes(
                transactionWitnessSet.to_bytes()
            )
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

const AssetValue = (lovelace: BigNum, policyIdHex: string, assetNameHex: string, amount: BigNum) => {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        const hoskyValue = Cardano.Value.new(lovelace);
        const hoskyMA = Cardano.MultiAsset.new();
        const hoskyAssets = Cardano.Assets.new();

        hoskyAssets.insert(
            Cardano.AssetName.new(fromHex(assetNameHex)),
            amount
        );

        hoskyMA.insert(
            Cardano.ScriptHash.from_bytes(fromHex(policyIdHex)),
            hoskyAssets
        );

        hoskyValue.set_multiasset(hoskyMA)
        return hoskyValue;
    }
}

const HoskySwapDatum = (pkh: string) => {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        const datumFields = Cardano.PlutusList.new();
        datumFields.add(Cardano.PlutusData.new_integer(Cardano.BigInt.from_str("1000000")));
        datumFields.add(AssetClassDatum("", "") as PlutusData);
        datumFields.add(AssetClassDatum("88672eaaf6f5c5fb59ffa5b978016207dbbf769014c6870d31adc4de", "484f534b59") as PlutusData);
        datumFields.add(Cardano.PlutusData.new_bytes(fromHex(pkh)));

        return Cardano.PlutusData.new_constr_plutus_data(
            Cardano.ConstrPlutusData.new(
                Cardano.Int.new_i32(0),
                datumFields
            )
        );
    }
}

const CreateTransactionBuilderAsync = async () => {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        let protocolParams = await GetProtocolProtocolParamsAsync();
        const txBuilder = Cardano.TransactionBuilder.new(
            Cardano.LinearFee.new(
                toBigNum(protocolParams.min_fee_a),
                toBigNum(protocolParams.min_fee_b)
            ),
            toBigNum("1000000"),
            toBigNum("500000000"),
            toBigNum("2000000"),
            parseInt("5000"),
            16384,
            5.77e-2,
            7.21e-5,
            Cardano.LanguageViews.new(fromHex(languageViews))
        );
        return txBuilder;
    }
}

const SimpleRedeemer = (index: number) => {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        const redeemerData = Cardano.PlutusData.new_constr_plutus_data(
            Cardano.ConstrPlutusData.new(
                Cardano.Int.new_i32(0),
                Cardano.PlutusList.new()
            )
        );

        return Cardano.Redeemer.new(
            Cardano.RedeemerTag.new_spend(),
            toBigNum(index),
            redeemerData,
            Cardano.ExUnits.new(
                Cardano.BigNum.from_str("7000000"),
                Cardano.BigNum.from_str("3000000000")
            )
        )
    }
}

const AssetClassDatum = (policyId: string, assetName: string) => {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        const datumFields = Cardano.PlutusList.new();
        datumFields.add(Cardano.PlutusData.new_bytes(fromHex(policyId)));
        datumFields.add(Cardano.PlutusData.new_bytes(fromHex(assetName)));

        return Cardano.PlutusData.new_constr_plutus_data(
            Cardano.ConstrPlutusData.new(
                Cardano.Int.new_i32(0),
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