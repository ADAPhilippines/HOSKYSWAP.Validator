import { TransactionUnspentOutput, BigNum, Vkeywitnesses, ScriptHash, AssetName, PlutusData, Ed25519KeyHash, BaseAddress, Redeemer, PlutusScripts, TransactionBuilder, Address, Value } from '@emurgo/cardano-serialization-lib-browser';
import CardanoLoader from './CardanoLoader';
import CardanoProtocolParameters from './Types/CardanoProtocolParam';
import { contractCbor } from "./contract";
import { languageViews } from './languageViews';

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

        const selfAddress = Cardano.Address.from_bytes(fromHex(await GetWalletAddressAsync()));
        const baseAddress = Cardano.BaseAddress.from_address(selfAddress) as BaseAddress;
        const pkh = toHex(baseAddress.payment_cred().to_keyhash()?.to_bytes() as Uint8Array);

        const transactionWitnessSet = Cardano.TransactionWitnessSet.new();
        const datumHash = Cardano.hash_plutus_data(HoskySwapDatum(pkh) as PlutusData);

        console.log("datumHash", toHex(datumHash.to_bytes()));
        console.log("pkh", pkh);

        const transactionInputs = Cardano.TransactionInputs.new();
        transactionInputs.add(
            Cardano.TransactionInput.new(
                Cardano.TransactionHash.from_bytes(fromHex("7f3c2aea756cd9fcaadc00623bf7b457ac4213353de935103b244f84ba5bed95")), 0
            )
        );

        const transctionOutputs = Cardano.TransactionOutputs.new();
        const contractOutput = Cardano.TransactionOutput.new(
            ContractAddress() as Address,
            Cardano.Value.new(toBigNum("12394200"))
        );

        contractOutput.set_data_hash(datumHash);

        transctionOutputs.add(contractOutput);
        transctionOutputs.add( Cardano.TransactionOutput.new(
            selfAddress,
            Cardano.Value.new(toBigNum("7105800"))
        ));

        const txBody = Cardano.TransactionBody.new(
            transactionInputs,
            transctionOutputs,
            toBigNum("500000"));

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
        const transactionWitnessSet = Cardano.TransactionWitnessSet.new();

        const selfAddress = Cardano.Address.from_bytes(fromHex(await GetWalletAddressAsync()));
        const baseAddress = Cardano.BaseAddress.from_address(selfAddress) as BaseAddress;
        const pkh = "3e4a2ec70fcef9e54c437a173714d1f82b96242379816bea3dd387dd";

        const scriptInput = Cardano.TransactionInput.new(
            Cardano.TransactionHash.from_bytes(fromHex("607fd494c917fd45a97659c7ae14a09bf03c150f2ca0581734c9c9c3162422ef")), 0
        );

        const transactionInputs = Cardano.TransactionInputs.new();
        transactionInputs.add(scriptInput);
        transactionInputs.add(
            Cardano.TransactionInput.new(
                Cardano.TransactionHash.from_bytes(fromHex("00a49dc9412ddc9b179821b7ec7adc9bc35b07939e74d620f295ca8181f69272")), 0
            )
        );

        const transactionOutputs = Cardano.TransactionOutputs.new();

        transactionOutputs.add(Cardano.TransactionOutput.new(
            Cardano.Address.from_bech32("addr_test1qqly5tk8pl80ne2vgdapwdc568uzh93yyducz6l28hfc0htkdqgkydtswrycyf9hruerftc8mwel9ck6pksvyszs968qhauh2s"),
            AssetValue(
                toBigNum("1500000"),
                "88672eaaf6f5c5fb59ffa5b978016207dbbf769014c6870d31adc4de",
                "484f534b59",
                toBigNum("10000000")
            ) as Value
        ));


        transactionOutputs.add(Cardano.TransactionOutput.new(
            Cardano.Address.from_bech32("addr_test1qp99g3msu76sg2g2xl996lqnvxkanygpkvf49g6evg693ehq5gyu2ypuw7k3lfmrpmsdk9qwnw9pw0vp7gg6rr5qkd9qqw0ela"),
            Cardano.Value.new(toBigNum("1388400"))
        ));

        transactionOutputs.add(Cardano.TransactionOutput.new(
            Cardano.Address.from_bech32("addr_test1qrnrqg4s73skqfyyj69mzr7clpe8s7ux9t8z6l55x2f2xuqra34p9pswlrq86nq63hna7p4vkrcrxznqslkta9eqs2nsmlqvnk"),
            Cardano.Value.new(toBigNum("10405800"))
        ));

        const requiredSigners = Cardano.Ed25519KeyHashes.new();
        requiredSigners.add(baseAddress.payment_cred().to_keyhash() as Ed25519KeyHash);

        const datum = HoskySwapDatum(pkh) as PlutusData;
        const datumList = Cardano.PlutusList.new();
        datumList.add(datum);

        const redeemers = Cardano.Redeemers.new();
        redeemers.add(SimpleRedeemer(1) as Redeemer);

        transactionWitnessSet.set_plutus_scripts(ContractScript() as PlutusScripts);
        transactionWitnessSet.set_plutus_data(Cardano.PlutusList.from_bytes(datumList.to_bytes()));
        transactionWitnessSet.set_redeemers(Cardano.Redeemers.from_bytes(redeemers.to_bytes()));

        const collateralUnspentTransactions = (await GetCollateralUnspentTransactionOutputAsync()) as TransactionUnspentOutput[];
        const collateralInputs = Cardano.TransactionInputs.new();
        collateralUnspentTransactions.forEach(c => collateralInputs.add(c.input()));

        const txBody = Cardano.TransactionBody.new(
            transactionInputs,
            transactionOutputs,
            toBigNum("600000")
        );

        const cost_model_vals = [150000, 150000, 150000, 150000, 150000, 150000, 150000, 150000, 150000, 32, 29773, 29773, 150000, 32, 150000, 150000, 150000, 32, 32, 150000, 150000, 4, 29773, 29773, 29773, 29773, 150000, 150000, 150000, 150000, 32, 1, 32, 150000, 32, 32, 32, 150000, 100, 100, 32, 32, 32, 4, 4, 32, 29773, 100, 32, 150000, 32, 29175, 100, 100, 100, 100, 32, 32, 32, 32, 1, 1, 150000, 32, 32, 29175, 82363, 100, 100, 1, 150000, 32, 0, 1000, 1000, 10000, 1, 32, 2477736, 1000, 1000, 1, 150000, 1, 2477736, 0, 1, 1, 8, 8, 1, 1326, 148000, 1, 197209, 1000, 150000, 150000, 150000, 1, 1000, 4, 1, 497, 1, 11218, 5000, 0, 1, 621, 150000, 148000, 1, 247, 150000, 1, 1, 0, 0, 136542, 0, 150000, 1, 248, 1, 1, 148000, 1, 1, 1, 1, 0, 150000, 179690, 61516, 148000, 1, 150000, 197209, 3345831, 396231, 0, 112536, 1, 0, 1, 1366, 1, 103599, 0, 0, 0, 1, 0, 0, 248, 145276, 118, 103599, 118, 425507, 118, 118, 425507, 425507, 425507];

        const costModel = Cardano.CostModel.new();
        cost_model_vals.forEach((x, i) => {
            if (Cardano !== null) {
                costModel.set(i, Cardano.Int.new_i32(x));
            }
        });

        const costModels = Cardano.Costmdls.new();
        costModels.insert(Cardano.Language.new_plutus_v1(), costModel);

        // set script data hash
        txBody.set_script_data_hash(
            Cardano.hash_script_data(redeemers, costModels, datumList)
        );

        txBody.set_collateral(collateralInputs);

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
                toBigNum("0"),
                datumFields
            )
        );
    }
}

// const CreateTransactionBuilderAsync = async () => {
//     let Cardano = CardanoSerializationLib();
//     if (Cardano !== null) {
//         let protocolParams = await GetProtocolProtocolParamsAsync();
//         const txBuilder = Cardano.TransactionBuilder.new(
//             Cardano.LinearFee.new(
//                 toBigNum(protocolParams.min_fee_a),
//                 toBigNum(protocolParams.min_fee_b)
//             ),
//             toBigNum("1000000"),
//             toBigNum("500000000"),
//             toBigNum("2000000"),
//             parseInt("5000"),
//             16384,
//             5.77e-2,
//             7.21e-5,
//             Cardano.LanguageViews.new(fromHex(languageViews))
//         );
//         return txBuilder;
//     }
// }

const SimpleRedeemer = (index: number) => {
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
            toBigNum(index),
            redeemerData,
            Cardano.ExUnits.new(
                Cardano.BigNum.from_str("1754991"),
                Cardano.BigNum.from_str("652356532")
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

const ContractAddress = () => {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        return Cardano.Address.from_bech32("addr_test1wp8qttdrd4qqvnpnuhmzlqlecnmt7wn3u4erlkncu3y9fhgx4yltu")
    }
}

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