import { HubConnection, HubConnectionBuilder } from '@microsoft/signalr';
import { Value, TransactionUnspentOutput, BigNum, Vkeywitnesses, ScriptHash, AssetName, PlutusData, Ed25519KeyHash, BaseAddress, Redeemer, PlutusScripts, TransactionBuilder, Address, TransactionOutputs, TransactionOutput } from './custom_modules/@emurgo/cardano-serialization-lib-browser';
import CardanoLoader from './CardanoLoader';
import CardanoProtocolParameters from './Types/CardanoProtocolParam';
import { contractCbor } from "./contract";
import { vtClaimContract } from "./vtClaimContract";
import { languageViews } from './languageViews'; import { CoinSelection, setCardanoSerializationLib as setCoinSelectionCardanoSerializationLib } from './coinSelection';
import { PlutusDataObject } from './Types/PlutusDataObject';
import { PlutusField, PlutusFieldType } from './Types/PlutusField';
import CardanoChainData from './Types/CardanoChainData';
import CardanoAsset from './Types/CardanoAsset';
import { vaporizeContract } from './vaporizeContract';

const BLOCKFROST_PROJECT_ID = "testnetIQtFV2rODiSJP0ROecSNx89tPRQ69mfN";
const CardanoSerializationLib = CardanoLoader.CardanoSerializationLib;

let btnSwap: HTMLButtonElement;
let btnBootstrap: HTMLButtonElement;
let btnWithdraw: HTMLButtonElement;
let btnCommit: HTMLButtonElement;
let btnProve: HTMLButtonElement;
let btnClaim: HTMLButtonElement;
let txtFrom: HTMLInputElement;
let txtTo: HTMLInputElement;
let txtPrice: HTMLInputElement;
let signalRConnection: HubConnection;
let hsVaporSignalRConnection: HubConnection;
let Orders: any[] = [];

let shadowHSUtxos: CardanoChainData[] = [];
let vrtUtxos: CardanoChainData[] = [];
let vtUtxos: CardanoChainData[] = [];
let MinUtxoLovelace = 3000000;

async function Main() {
    signalRConnection = new HubConnectionBuilder()
        .withUrl("http://localhost:1338/swap")
        .build();
    await signalRConnection.start();

    await signalRConnection.send("BuyOrderUpdate");

    signalRConnection.on("BuyOrderUpdate", (orders: any[]) => {
        console.log("buyOrders", orders);
        Orders = orders;

        const buyTable = (document.getElementById("buyTable") as HTMLTableElement);
        buyTable.querySelectorAll(".buyOrder").forEach(o => o.remove());
        orders.forEach(o => {
            const tr = document.createElement("tr");
            tr.className = "buyOrder";
            const tdPrice = document.createElement("td");
            const tdTotal = document.createElement("td");
            const tdAction = document.createElement("td");
            const btnSell = document.createElement("button");

            btnSell.innerText = "Sell";
            btnSell.onclick = () => ExecuteSell(o);

            tdAction.appendChild(btnSell);

            tr.appendChild(tdPrice);
            tr.appendChild(tdTotal);
            tr.appendChild(tdAction);

            tdPrice.innerText = o.Fields[0].Value;
            tdTotal.innerText = o.Amounts[0].Quantity;
            (document.getElementById("buyTable") as HTMLTableElement).appendChild(tr);
        });
    });

    signalRConnection.on("ChainDataUpdated", async () => {
        console.log("hoskyswap chain data updated!");
        await signalRConnection.send("BuyOrderUpdate");
    });

    hsVaporSignalRConnection = new HubConnectionBuilder()
    .withUrl("http://localhost:1338/vapor")
    .build();
    
    await hsVaporSignalRConnection.start();

    await hsVaporSignalRConnection.send("VTClaimGetShUtxos", ["48595045534b554c4c303030315f5348", "48595045534b554c4c303030315f5349"]);
    await hsVaporSignalRConnection.send("VTClaimGetVrtUtxos");

    hsVaporSignalRConnection.on("ChainDataUpdated", async () => {
        console.log("Vapor chain data updated!");
        await hsVaporSignalRConnection.send("VTClaimGetShUtxos", ["48595045534b554c4c303030315f5348"]);
        await hsVaporSignalRConnection.send("VTClaimGetVrtUtxos");
    });

    hsVaporSignalRConnection.on("VTClaimSendShUtxos", async (utxos: CardanoChainData[]) => {
        console.log("shadow HS utxos: ", utxos);
        (document.getElementById("shCountCell") as HTMLTableElement).innerHTML = utxos.length.toString();
        shadowHSUtxos = utxos;
    });

    hsVaporSignalRConnection.on("VTClaimSendVrtUtxos", (utxos: CardanoChainData[]) => {
        console.log("VRT utxos: ", utxos);
        (document.getElementById("vrtCountCell") as HTMLTableElement).innerHTML = utxos.length.toString();
        vrtUtxos = utxos;
    });

    hsVaporSignalRConnection.on("VTClaimSendVTUtxo", (utxo: CardanoChainData) => {
        console.log("VT utxos: ", utxo);
        vtUtxos = [utxo];
        (document.getElementById("vtCountCell") as HTMLTableElement).innerHTML = vtUtxos.length.toString();
    });

    hsVaporSignalRConnection.on("VTClaimShUtxosUpdated", async () => {
        await hsVaporSignalRConnection.send("VTClaimGetShUtxos", ["48595045534b554c4c303030315f5348"]);
    });

    hsVaporSignalRConnection.on("VTClaimVrtUtxosUpdated", async () => {
        await hsVaporSignalRConnection.send("VTClaimGetVrtUtxos");
    });

    hsVaporSignalRConnection.on("TxSumbitSuccess", (txId) => {
        console.log("Tx submitted:", txId);
    });

    await CardanoLoader.LoadAsync();
    btnSwap = document.getElementById("btnSwap") as HTMLButtonElement;
    txtFrom = document.getElementById("txtFrom") as HTMLInputElement;
    txtTo = document.getElementById("txtTo") as HTMLInputElement;
    txtPrice = document.getElementById("txtPrice") as HTMLInputElement;
    btnBootstrap = document.getElementById("btnBootstrap") as HTMLButtonElement;
    btnWithdraw = document.getElementById("btnWithdraw") as HTMLButtonElement;
    btnCommit = document.getElementById("btnCommit") as HTMLButtonElement;
    btnProve = document.getElementById("btnProve") as HTMLButtonElement;
    btnClaim = document.getElementById("btnClaim") as HTMLButtonElement;

    btnSwap.addEventListener("click", ExecuteSwap);
    txtFrom.addEventListener("keyup", OnFromChange);
    txtTo.addEventListener("keyup", OnFromChange);
    btnBootstrap.addEventListener("click", VTClaimBootstrapAsync);
    btnWithdraw.addEventListener("click", VTClaimPulloutAsync);

    btnCommit.addEventListener("click", async () => await SendVTClaimCommitTxAsync());
    btnProve.addEventListener("click", async () => await SendVTClaimProveTxAsync());
    btnClaim.addEventListener("click", async () => await SendVTClaimWithdrawTxAsync());
}

async function SendVTClaimCommitTxAsync()
{
    let skullNameHex = toHex(Buffer.from("HYPESKULL0001", "ascii"));
    let shUtxo = GetShadowHSData(skullNameHex + "5f5348") as CardanoChainData;
    await VTClaimCommitAsync(skullNameHex, shUtxo, vrtUtxos[0]);
}

async function SendVTClaimProveTxAsync()
{
    await VTClaimProveAsync(vrtUtxos[0]);
}

async function SendVTClaimWithdrawTxAsync()
{
    await VTClaimWithdrawAsync(vrtUtxos[0], vtUtxos[0]);
}

async function ExecuteSell(order: any) {
    await BuildSwapTxAsync(order);
}

async function ExecuteSwap() {
    CalculatePrice();
    await BuildOfferTxAsync();
}

function OnFromChange() {
    CalculatePrice();
}

function CalculatePrice() {
    try {
        const fromValue = parseInt(txtFrom.value);
        const toValue = parseInt(txtTo.value);
        txtPrice.value = (toValue / fromValue).toString();
    }
    catch (e) {
        console.log(e);
    }
}

async function ExecuteContract() {
    try {
        // await BuildOfferTxAsync();
        // await BuildSwapTxAsync();
    } catch (e) {
        console.dir(e);
    }
}

async function BuildOfferTxAsync() {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        setCoinSelectionCardanoSerializationLib(Cardano);
        const protocolParameters = await GetProtocolProtocolParamsAsync();
        CoinSelection.setProtocolParameters(
            protocolParameters.min_utxo.toString(),
            protocolParameters.min_fee_a.toString(),
            protocolParameters.min_fee_b.toString(),
            protocolParameters.max_tx_size.toString()
        );

        if (!await window.cardano.isEnabled()) await window.cardano.enable();
        const txBuilder = await CreateTransactionBuilderAsync() as TransactionBuilder;
        const selfAddress = Cardano.Address.from_bytes(fromHex(await GetWalletAddressAsync()));
        const baseAddress = Cardano.BaseAddress.from_address(selfAddress) as BaseAddress;
        const pkh = toHex(baseAddress.payment_cred().to_keyhash()?.to_bytes() as Uint8Array);

        const transactionWitnessSet = Cardano.TransactionWitnessSet.new();
        const hoskyDatumObject = HoskySwapDatum(pkh, parseFloat(txtPrice.value)) as PlutusDataObject;
        const datumHash = Cardano.hash_plutus_data(ToPlutusData(hoskyDatumObject) as PlutusData);

        console.log("datumHash", toHex(datumHash.to_bytes()));
        console.log("pkh", pkh);
        console.log("value", GetContractOutput()?.coin().to_str());
        console.log("datumObj", hoskyDatumObject);

        const contractOutput = Cardano.TransactionOutput.new(
            ContractAddress() as Address,
            GetContractOutput() as Value
        );
        contractOutput.set_data_hash(datumHash);
        const transactionOutputs = Cardano.TransactionOutputs.new();
        transactionOutputs.add(contractOutput);

        const utxos = await window.cardano.getUtxos();
        const csResult = CoinSelection.randomImprove(
            utxos.map(utxo => Cardano?.TransactionUnspentOutput.from_bytes(fromHex(utxo)) as TransactionUnspentOutput),
            transactionOutputs,
            8
        );

        csResult.inputs.forEach((utxo) => {
            txBuilder.add_input(
                utxo.output().address(),
                utxo.input(),
                utxo.output().amount()
            );
        });

        txBuilder.add_output(contractOutput);

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

        console.log("full tx size", signedTx.to_bytes().length);
        await signalRConnection.send("SubmitOrderTx", toHex(signedTx.to_bytes()), JSON.stringify(hoskyDatumObject));
    }
}

async function BuildSwapTxAsync(order: any) {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {

        setCoinSelectionCardanoSerializationLib(Cardano);
        const protocolParameters = await GetProtocolProtocolParamsAsync();
        CoinSelection.setProtocolParameters(
            protocolParameters.min_utxo.toString(),
            protocolParameters.min_fee_a.toString(),
            protocolParameters.min_fee_b.toString(),
            protocolParameters.max_tx_size.toString()
        );

        const txBuilder = await CreateTransactionBuilderAsync() as TransactionBuilder;
        const transactionWitnessSet = Cardano.TransactionWitnessSet.new();

        const selfAddress = Cardano.Address.from_bytes(fromHex(await GetWalletAddressAsync()));
        const baseAddress = Cardano.BaseAddress.from_address(selfAddress) as BaseAddress;

        const pkh = GetPkhFromOrder(order);
        console.log("pkh", pkh);

        const utxoLovelaceAmount = parseInt(order.Amounts[0].Quantity);
        const rate = GetRateFromOrder(order);
        
        const scriptUtxo = Cardano.TransactionUnspentOutput.new(
            Cardano.TransactionInput.new(
                Cardano.TransactionHash.from_bytes(fromHex(order.TxId)), order.TxIdx
            ),
            Cardano.TransactionOutput.new(
                ContractAddress() as Address,
                Cardano.Value.new(toBigNum(utxoLovelaceAmount))
            )
        );

        const utxos = await window.cardano.getUtxos();
        const outputs: TransactionOutput[] = [
            Cardano.TransactionOutput.new(
                Cardano.Address.from_bech32(order.OwnerAddress),
                AssetValue(
                    toBigNum(2000000),
                    "88672eaaf6f5c5fb59ffa5b978016207dbbf769014c6870d31adc4de",
                    "484f534b59",
                    toBigNum((utxoLovelaceAmount - 2000000 - 694200 - 300000) * (rate / 1000000))
                ) as Value
            ),
            Cardano.TransactionOutput.new(
                Cardano.Address.from_bech32("addr_test1qrh2hpl5p222c5nk4ect8x6swfk3ttpmkh004dlrharef29n93agwcyekkqn83vxngd7d5rjzpf4eyn2cas4pxqj5kmqpzvlg9"),
                Cardano.Value.new(toBigNum("1388400"))
            )
        ];

        const transactionOutputs = Cardano.TransactionOutputs.new();

        outputs.forEach(output => transactionOutputs.add(output));

        const csResult = CoinSelection.randomImprove(
            utxos.map(utxo => Cardano?.TransactionUnspentOutput.from_bytes(fromHex(utxo)) as TransactionUnspentOutput),
            transactionOutputs,
            8,
            [scriptUtxo]
        );
            

        csResult.inputs.forEach((utxo) => {
            txBuilder.add_input(
                utxo.output().address(),
                utxo.input(),
                utxo.output().amount()
            );
        });

        const scriptInputIndex = txBuilder.index_of_input(scriptUtxo.input());

        outputs.forEach(output => txBuilder.add_output(output));

        const requiredSigners = Cardano.Ed25519KeyHashes.new();
        requiredSigners.add(baseAddress.payment_cred().to_keyhash() as Ed25519KeyHash);
        txBuilder.set_required_signers(requiredSigners);

        const datumObj = HoskySwapDatum(pkh, rate);
        const datum = ToPlutusData(datumObj as PlutusDataObject) as PlutusData;

        console.log(toHex(Cardano.hash_plutus_data(datum).to_bytes()));

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

        console.log("full tx size", signedTx.to_bytes().length);
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

const GetContractOutput = () => {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        let fromAmount = parseInt(txtFrom.value);
        if (GetFromUnit() === "ada") {
            fromAmount *= 1000000;
            fromAmount += 2000000;
            fromAmount += 694200;
            fromAmount += 300000;
            return Cardano.Value.new(toBigNum(fromAmount));
        }
        else {
            return AssetValue(
                toBigNum("2000000"),
                "88672eaaf6f5c5fb59ffa5b978016207dbbf769014c6870d31adc4de",
                "484f534b59",
                toBigNum(fromAmount)
            );
        }
    }
}

const GetFromUnit = () => {
    const selFrom = document.getElementById("selFrom") as HTMLSelectElement;
    return selFrom.value;
};

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

const ToPlutusData = (plutusDataObj: PlutusDataObject) => {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {

        const datumFields = Cardano.PlutusList.new();
        plutusDataObj.Fields.sort((a, b) => a.Index - b.Index);
        plutusDataObj.Fields.forEach(f => {
            if (Cardano === null) return;
            switch (f.Type) {
                case PlutusFieldType.Integer:
                    datumFields.add(Cardano.PlutusData.new_integer(Cardano.BigInt.from_str(f.Value.toString())));
                    break;
                case PlutusFieldType.Bytes:
                    datumFields.add(Cardano.PlutusData.new_bytes(fromHex(f.Value)));
                    break;
                case PlutusFieldType.Data:
                    datumFields.add(ToPlutusData(f.Value) as PlutusData);
                    break;
                case PlutusFieldType.List:
                    let elements = Cardano.PlutusList.new();
                    f.Value.forEach((el: any) => {
                        elements.add(ToPlutusData(el) as PlutusData)
                    });
                    datumFields.add(Cardano.PlutusData.new_list(elements));
                    break;
            }
        })

        return Cardano.PlutusData.new_constr_plutus_data(
            Cardano.ConstrPlutusData.new(
                Cardano.Int.new_i32(plutusDataObj.ConstructorIndex),
                datumFields
            )
        );
    }
}

const HoskySwapDatum = (pkh: string, rate: number) => {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {

        const hoskySwapDatum = new PlutusDataObject(0);
        hoskySwapDatum.Fields = [
            {
                Index: 0,
                Type: PlutusFieldType.Integer,
                Key: "siRate",
                Value: rate
            } as PlutusField,
            {
                Index: 0,
                Type: PlutusFieldType.Data,
                Key: "siFromAsset",
                Value: AssetClassDatum("", "")
            } as PlutusField,
            {
                Index: 0,
                Type: PlutusFieldType.Data,
                Key: "siToAsset",
                Value: AssetClassDatum("88672eaaf6f5c5fb59ffa5b978016207dbbf769014c6870d31adc4de", "484f534b59")
            } as PlutusField,
            {
                Index: 0,
                Type: PlutusFieldType.Bytes,
                Key: "siSeller",
                Value: pkh
            } as PlutusField
        ];

        return hoskySwapDatum;
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
                Cardano.BigNum.from_str("2201043"),
                Cardano.BigNum.from_str("772999166")
            )
        )
    }
}

const AssetClassDatum = (policyId: string, assetName: string) => {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {

        const assetDatum = new PlutusDataObject(0);
        assetDatum.Fields = [
            {
                Index: 0,
                Type: PlutusFieldType.Bytes,
                Key: "CurrencySymbol",
                Value: policyId
            } as PlutusField,
            {
                Index: 1,
                Type: PlutusFieldType.Bytes,
                Key: "TokenName",
                Value: assetName
            } as PlutusField
        ];
        return assetDatum;
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
        return Cardano.Address.from_bech32("addr_test1wp9h4547t2ndcv4n7htayhakm5st9ypeerf76u0rm2hmmfs0pgd0t")
    }
}

const VTClaimContractAddress = () => {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        return Cardano.Address.from_bech32("addr_test1wq4wzuzkvgjetxuyam4cps3ke54jtjm3fm6snvpq0qwu5rcejx5z7")
    }
}

const VaporizeContractAddress = () => {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        return Cardano.Address.from_bech32("addr_test1wpc8q30gp4l64s6hmlnypw8g95zvhw22603jv2stcge0g6gskve2k")
    }
}

const VaporAdminAddress = () => {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        return Cardano.Address.from_bech32("addr_test1qrdlvngtwzufm26cwtdcdl6a8yau2j8u6wuwhfsxzr9ws0rvwmu9kj7uxdwdnhsxl5mue3sqvfhctwl2kcnh8yy03vvqsdz0m9")
    }
}

async function VTClaimBootstrapAsync() {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        setCoinSelectionCardanoSerializationLib(Cardano);
        const protocolParameters = await GetProtocolProtocolParamsAsync();
        CoinSelection.setProtocolParameters(
            protocolParameters.min_utxo.toString(),
            protocolParameters.min_fee_a.toString(),
            protocolParameters.min_fee_b.toString(),
            protocolParameters.max_tx_size.toString()
        );

        if (!await window.cardano.isEnabled()) await window.cardano.enable();
        const txBuilder = await CreateTransactionBuilderAsync() as TransactionBuilder;
        const selfAddress = Cardano.Address.from_bytes(fromHex(await GetWalletAddressAsync()));

        const transactionWitnessSet = Cardano.TransactionWitnessSet.new();
        const shDatumObject = VTClaimShDatum() as PlutusDataObject;
        const vrtDatumObject = VTClaimVrtDatum("") as PlutusDataObject;
        const vtDatumObject = VTClaimVtDatum("d3c81d34ac1ebd82c9f7afe1a19b2ccb5200b5e82830de42d8545f251c08c77a") as PlutusDataObject;

        const shDatumHash = Cardano.hash_plutus_data(ToPlutusData(shDatumObject) as PlutusData);
        const vrtDatumHash = Cardano.hash_plutus_data(ToPlutusData(vrtDatumObject) as PlutusData);
        const vtDatumHash = Cardano.hash_plutus_data(ToPlutusData(vtDatumObject) as PlutusData);

        console.log("shDatumHash", toHex(shDatumHash.to_bytes()));
        console.log("shDatumObject", shDatumObject);

        console.log("vrtDatumHash", toHex(vrtDatumHash.to_bytes()));
        console.log("vrtDatumObject", vrtDatumObject);

        console.log("vtDatumHash", toHex(vtDatumHash.to_bytes()));
        console.log("vtDatumObject", vtDatumObject);

        const shOutput = Cardano.TransactionOutput.new(
            VTClaimContractAddress() as Address,
            GetHypeNftsOutput(["48595045534b554c4c303030315f5348"]) as Value
        );
        shOutput.set_data_hash(shDatumHash);

        const vrtOutput = Cardano.TransactionOutput.new(
            VTClaimContractAddress() as Address,
            GetHypeNftsOutput(["48595045534B554C4C535F5652545F30303031"]) as Value
        );
        vrtOutput.set_data_hash(vrtDatumHash);

        const vtOutput = Cardano.TransactionOutput.new(
            VTClaimContractAddress() as Address,
            GetHypeNftsOutput(["48595045534B554C4C535F56545F53505F4545", "48595045534B554C4C535F56545F4D4B5F43", "48595045534B554C4C535F56545F4E554747455453"]) as Value
        );
        vtOutput.set_data_hash(vtDatumHash);

        const transactionOutputs = Cardano.TransactionOutputs.new();
        transactionOutputs.add(shOutput);
        transactionOutputs.add(vrtOutput);
        transactionOutputs.add(vtOutput);

        const utxos = await window.cardano.getUtxos();
        const csResult = CoinSelection.randomImprove(
            utxos.map(utxo => Cardano?.TransactionUnspentOutput.from_bytes(fromHex(utxo)) as TransactionUnspentOutput),
            transactionOutputs,
            8
        );

        csResult.inputs.forEach((utxo) => {
            txBuilder.add_input(
                utxo.output().address(),
                utxo.input(),
                utxo.output().amount()
            );
        });

        txBuilder.add_output(shOutput);
        txBuilder.add_output(vrtOutput);
        txBuilder.add_output(vtOutput);

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

        console.log("full tx size", signedTx.to_bytes().length);
        await hsVaporSignalRConnection.send("SubmitVTClaimTx", toHex(signedTx.to_bytes()), 
        [shDatumObject,vrtDatumObject,vtDatumObject]);

    }
}

async function VTClaimCommitAsync(skullName: string, shUtxo: CardanoChainData, vrtUtxo: CardanoChainData) {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        console.log("shadow utxo", shUtxo);
        console.log("vrt utxo", vrtUtxo);

        setCoinSelectionCardanoSerializationLib(Cardano);
        const protocolParameters = await GetProtocolProtocolParamsAsync();
        CoinSelection.setProtocolParameters(
            protocolParameters.min_utxo.toString(),
            protocolParameters.min_fee_a.toString(),
            protocolParameters.min_fee_b.toString(),
            protocolParameters.max_tx_size.toString()
        );

        const txBuilder = await CreateTransactionBuilderAsync() as TransactionBuilder;
        const transactionWitnessSet = Cardano.TransactionWitnessSet.new();

        const selfAddress = Cardano.Address.from_bytes(fromHex(await GetWalletAddressAsync()));
        const baseAddress = Cardano.BaseAddress.from_address(selfAddress) as BaseAddress;
        const pkh = toHex(baseAddress.payment_cred().to_keyhash()?.to_bytes() as Uint8Array);
        
        const scriptUtxos = [
            Cardano.TransactionUnspentOutput.new(
                Cardano.TransactionInput.new(
                    Cardano.TransactionHash.from_bytes(fromHex(shUtxo.TxId)), 
                    shUtxo.TxIdx
                ),
                GetOutputFromChainData(VTClaimContractAddress() as Address, shUtxo) as TransactionOutput
            ),
            Cardano.TransactionUnspentOutput.new(
                Cardano.TransactionInput.new(
                    Cardano.TransactionHash.from_bytes(fromHex(vrtUtxo.TxId)), 
                    vrtUtxo.TxIdx
                ),
                GetOutputFromChainData(VTClaimContractAddress() as Address, vrtUtxo) as TransactionOutput
            )
        ]
        

        const newVrtDatumObject = VTClaimVrtDatum(pkh) as PlutusDataObject;
        const newVrtDatumHash = Cardano.hash_plutus_data(ToPlutusData(newVrtDatumObject) as PlutusData);
        const vrtTokenOut = GetOutputFromChainData(VTClaimContractAddress() as Address, vrtUtxo) as TransactionOutput
        vrtTokenOut.set_data_hash(newVrtDatumHash);

        const outputs: TransactionOutput[] = [
            Cardano.TransactionOutput.new(
                selfAddress,
                GetHypeNftsOutput([skullName]) as Value
            ),
            GetOutputFromChainData(VaporAdminAddress() as Address, shUtxo) as TransactionOutput,
            vrtTokenOut
        ];

        const transactionOutputs = Cardano.TransactionOutputs.new();

        outputs.forEach(output => transactionOutputs.add(output));

        const utxos = await window.cardano.getUtxos();
        const csResult = CoinSelection.randomImprove(
            utxos.map(utxo => Cardano?.TransactionUnspentOutput.from_bytes(fromHex(utxo)) as TransactionUnspentOutput),
            transactionOutputs,
            8,
            scriptUtxos
        );

        csResult.inputs.forEach((utxo) => {
            txBuilder.add_input(
                utxo.output().address(),
                utxo.input(),
                utxo.output().amount()
            );
        });

        outputs.forEach(output => txBuilder.add_output(output));

        const requiredSigners = Cardano.Ed25519KeyHashes.new();
        requiredSigners.add(baseAddress.payment_cred().to_keyhash() as Ed25519KeyHash);
        txBuilder.set_required_signers(requiredSigners);

        const shDatumObject = VTClaimShDatum() as PlutusDataObject;
        const vrtDatumObject = VTClaimVrtDatum(vrtUtxo.Fields[0].Value as string) as PlutusDataObject;

        const shDatum = ToPlutusData(shDatumObject) as PlutusData;
        const vrtDatum = ToPlutusData(vrtDatumObject) as PlutusData;

        const datumList = Cardano.PlutusList.new();
        datumList.add(shDatum);
        datumList.add(vrtDatum);
        datumList.add(ToPlutusData(newVrtDatumObject as PlutusDataObject) as PlutusData)

        const redeemers = Cardano.Redeemers.new();
        scriptUtxos.forEach(utxo => {
            const scriptInputIndex = txBuilder.index_of_input(utxo.input());
            if(toHex(utxo.input().transaction_id().to_bytes()) == shUtxo.TxId && 
                utxo.input().index() == shUtxo.TxIdx)
                redeemers.add(VTClaimCommitSkullRedeemer(scriptInputIndex) as Redeemer);
            else
                redeemers.add(VTClaimCommitRandomRedeemer(scriptInputIndex) as Redeemer);
        });

        txBuilder.set_plutus_scripts(VTClaimContractScript() as PlutusScripts);
        txBuilder.set_plutus_data(Cardano.PlutusList.from_bytes(datumList.to_bytes()));
        txBuilder.set_redeemers(Cardano.Redeemers.from_bytes(redeemers.to_bytes()));

        transactionWitnessSet.set_plutus_scripts(VTClaimContractScript() as PlutusScripts);
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

        console.log("full tx size", signedTx.to_bytes().length);
        console.log("submitting tx");
        await hsVaporSignalRConnection.send("SubmitVrtTx", toHex(signedTx.to_bytes()), 
        [newVrtDatumObject]);
    }
}

async function VTClaimProveAsync(vrtUtxo: CardanoChainData) {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        console.log("vrt utxo", vrtUtxo);

        setCoinSelectionCardanoSerializationLib(Cardano);
        const protocolParameters = await GetProtocolProtocolParamsAsync();
        CoinSelection.setProtocolParameters(
            protocolParameters.min_utxo.toString(),
            protocolParameters.min_fee_a.toString(),
            protocolParameters.min_fee_b.toString(),
            protocolParameters.max_tx_size.toString()
        );

        const txBuilder = await CreateTransactionBuilderAsync() as TransactionBuilder;
        const transactionWitnessSet = Cardano.TransactionWitnessSet.new();

        const selfAddress = Cardano.Address.from_bytes(fromHex(await GetWalletAddressAsync()));
        const baseAddress = Cardano.BaseAddress.from_address(selfAddress) as BaseAddress;
        const pkh = toHex(baseAddress.payment_cred().to_keyhash()?.to_bytes() as Uint8Array);
        
        const scriptUtxos = [
            Cardano.TransactionUnspentOutput.new(
                Cardano.TransactionInput.new(
                    Cardano.TransactionHash.from_bytes(fromHex(vrtUtxo.TxId)), 
                    vrtUtxo.TxIdx
                ),
                GetOutputFromChainData(VTClaimContractAddress() as Address, vrtUtxo) as TransactionOutput
            )
        ]

        const vrtDatumObject = VTClaimVrtDatum(pkh) as PlutusDataObject;
        const vrtDatum = ToPlutusData(vrtDatumObject) as PlutusData;
        const vrtDatumHash = Cardano.hash_plutus_data(vrtDatum);
        const vrtTokenOut = GetOutputFromChainData(VTClaimContractAddress() as Address, vrtUtxo) as TransactionOutput
        vrtTokenOut.set_data_hash(vrtDatumHash);

        const outputs: TransactionOutput[] = [
            vrtTokenOut
        ];

        const transactionOutputs = Cardano.TransactionOutputs.new();

        outputs.forEach(output => transactionOutputs.add(output));

        const utxos = await window.cardano.getUtxos();
        const csResult = CoinSelection.randomImprove(
            utxos.map(utxo => Cardano?.TransactionUnspentOutput.from_bytes(fromHex(utxo)) as TransactionUnspentOutput),
            transactionOutputs,
            8,
            scriptUtxos
        );

        csResult.inputs.forEach((utxo) => {
            txBuilder.add_input(
                utxo.output().address(),
                utxo.input(),
                utxo.output().amount()
            );
        });

        outputs.forEach(output => txBuilder.add_output(output));

        const requiredSigners = Cardano.Ed25519KeyHashes.new();
        requiredSigners.add(baseAddress.payment_cred().to_keyhash() as Ed25519KeyHash);
        txBuilder.set_required_signers(requiredSigners);

        const datumList = Cardano.PlutusList.new();
        datumList.add(vrtDatum);

        const redeemers = Cardano.Redeemers.new();

        const scriptInputIndex = txBuilder.index_of_input(scriptUtxos[0].input());
        redeemers.add(VTClaimProveRedeemer(scriptInputIndex) as Redeemer);

        txBuilder.set_plutus_scripts(VTClaimContractScript() as PlutusScripts);
        txBuilder.set_plutus_data(Cardano.PlutusList.from_bytes(datumList.to_bytes()));
        txBuilder.set_redeemers(Cardano.Redeemers.from_bytes(redeemers.to_bytes()));

        transactionWitnessSet.set_plutus_scripts(VTClaimContractScript() as PlutusScripts);
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

        console.log("full tx size", signedTx.to_bytes().length);
        console.log("submitting tx");
        await hsVaporSignalRConnection.send("SubmitVrtTx", toHex(signedTx.to_bytes()), 
        [vrtDatumObject]);
    }
}

async function VTClaimWithdrawAsync(vrtUtxo: CardanoChainData, vtUtxo: CardanoChainData) {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        console.log("vrt utxo", vrtUtxo);
        console.log("vt utxo", vtUtxo);

        setCoinSelectionCardanoSerializationLib(Cardano);
        const protocolParameters = await GetProtocolProtocolParamsAsync();
        CoinSelection.setProtocolParameters(
            protocolParameters.min_utxo.toString(),
            protocolParameters.min_fee_a.toString(),
            protocolParameters.min_fee_b.toString(),
            protocolParameters.max_tx_size.toString()
        );

        const txBuilder = await CreateTransactionBuilderAsync() as TransactionBuilder;
        const transactionWitnessSet = Cardano.TransactionWitnessSet.new();

        const selfAddress = Cardano.Address.from_bytes(fromHex(await GetWalletAddressAsync()));
        const baseAddress = Cardano.BaseAddress.from_address(selfAddress) as BaseAddress;
        const pkh = toHex(baseAddress.payment_cred().to_keyhash()?.to_bytes() as Uint8Array);

        const scriptUtxos = [
            Cardano.TransactionUnspentOutput.new(
                Cardano.TransactionInput.new(
                    Cardano.TransactionHash.from_bytes(fromHex(vrtUtxo.TxId)), 
                    vrtUtxo.TxIdx
                ),
                GetOutputFromChainData(VTClaimContractAddress() as Address, vrtUtxo) as TransactionOutput
            ),
            Cardano.TransactionUnspentOutput.new(
                Cardano.TransactionInput.new(
                    Cardano.TransactionHash.from_bytes(fromHex(vtUtxo.TxId)), 
                    vtUtxo.TxIdx
                ),
                GetOutputFromChainData(VTClaimContractAddress() as Address, vtUtxo) as TransactionOutput
            )
        ]

        const outputs: TransactionOutput[] = [
            GetOutputFromChainData(VaporAdminAddress() as Address, vrtUtxo, MinUtxoLovelace) as TransactionOutput,
            GetOutputFromChainData(selfAddress as Address, vtUtxo) as TransactionOutput
        ];

        const transactionOutputs = Cardano.TransactionOutputs.new();

        outputs.forEach(output => transactionOutputs.add(output));

        const utxos = await window.cardano.getUtxos();
        const csResult = CoinSelection.randomImprove(
            utxos.map(utxo => Cardano?.TransactionUnspentOutput.from_bytes(fromHex(utxo)) as TransactionUnspentOutput),
            transactionOutputs,
            8,
            scriptUtxos
        );

        csResult.inputs.forEach((utxo) => {
            txBuilder.add_input(
                utxo.output().address(),
                utxo.input(),
                utxo.output().amount()
            );
        });

        outputs.forEach(output => txBuilder.add_output(output));

        const requiredSigners = Cardano.Ed25519KeyHashes.new();
        requiredSigners.add(baseAddress.payment_cred().to_keyhash() as Ed25519KeyHash);
        txBuilder.set_required_signers(requiredSigners);

        const vrtDatumObject = VTClaimVrtDatum(pkh) as PlutusDataObject;
        const vrtDatum = ToPlutusData(vrtDatumObject) as PlutusData;

        const vtDatumObject = VTClaimVtDatum(vtUtxo.Fields[0].Value as string) as PlutusDataObject;
        const vtDatum = ToPlutusData(vtDatumObject) as PlutusData;

        const datumList = Cardano.PlutusList.new();
        datumList.add(vtDatum);
        datumList.add(vrtDatum);

        const redeemers = Cardano.Redeemers.new();

        scriptUtxos.forEach(utxo => {
            const scriptInputIndex = txBuilder.index_of_input(utxo.input());
            if(toHex(utxo.input().transaction_id().to_bytes()) == vrtUtxo.TxId && 
                utxo.input().index() == vrtUtxo.TxIdx)
                redeemers.add(VTClaimUseRandomRedeemer(scriptInputIndex) as Redeemer);
            else
                redeemers.add(VTClaimWithdrawRedeemer(scriptInputIndex) as Redeemer);
        });

        txBuilder.set_plutus_scripts(VTClaimContractScript() as PlutusScripts);
        txBuilder.set_plutus_data(Cardano.PlutusList.from_bytes(datumList.to_bytes()));
        txBuilder.set_redeemers(Cardano.Redeemers.from_bytes(redeemers.to_bytes()));

        transactionWitnessSet.set_plutus_scripts(VTClaimContractScript() as PlutusScripts);
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

        console.log("full tx size", signedTx.to_bytes().length);
        console.log("submitting tx");
        await hsVaporSignalRConnection.send("SubmitVTClaimTx", toHex(signedTx.to_bytes()), []);
    }
}

async function VTClaimPulloutAsync() {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {

        setCoinSelectionCardanoSerializationLib(Cardano);
        const protocolParameters = await GetProtocolProtocolParamsAsync();
        CoinSelection.setProtocolParameters(
            protocolParameters.min_utxo.toString(),
            protocolParameters.min_fee_a.toString(),
            protocolParameters.min_fee_b.toString(),
            protocolParameters.max_tx_size.toString()
        );

        const txBuilder = await CreateTransactionBuilderAsync() as TransactionBuilder;
        const transactionWitnessSet = Cardano.TransactionWitnessSet.new();

        const selfAddress = Cardano.Address.from_bytes(fromHex(await GetWalletAddressAsync()));
        const baseAddress = Cardano.BaseAddress.from_address(selfAddress) as BaseAddress;

        const shOutput = Cardano.TransactionOutput.new(
            VTClaimContractAddress() as Address,
            GetHypeNftsOutput(["48595045534b554c4c303030315f5348"]) as Value
        );

        const vrtOutput = Cardano.TransactionOutput.new(
            VTClaimContractAddress() as Address,
            GetHypeNftsOutput(["48595045534B554C4C535F5652545F30303031"]) as Value
        );

        const vtOutput = Cardano.TransactionOutput.new(
            VTClaimContractAddress() as Address,
            GetHypeNftsOutput([
                "48595045534B554C4C535F56545F53505F4545", 
                "48595045534B554C4C535F56545F4D4B5F43", 
                "48595045534B554C4C535F56545F4E554747455453"]) as Value
        );
        
        const scriptUtxos = [
            Cardano.TransactionUnspentOutput.new(
                Cardano.TransactionInput.new(
                    Cardano.TransactionHash.from_bytes(fromHex(shadowHSUtxos[0].TxId)), 
                    shadowHSUtxos[0].TxIdx
                ),
                shOutput
            ),
            Cardano.TransactionUnspentOutput.new(
                Cardano.TransactionInput.new(
                    Cardano.TransactionHash.from_bytes(fromHex(vrtUtxos[0].TxId)), 
                    vrtUtxos[0].TxIdx
                ),
                vrtOutput
            ),
            Cardano.TransactionUnspentOutput.new(
                Cardano.TransactionInput.new(
                    Cardano.TransactionHash.from_bytes(fromHex(vtUtxos[0].TxId)), 
                    vtUtxos[0].TxIdx
                ),
                vtOutput
            ),
        ]

        const utxos = await window.cardano.getUtxos();
        const outputs: TransactionOutput[] = [
            Cardano.TransactionOutput.new(
                selfAddress,
                GetHypeNftsOutput(["48595045534b554c4c303030315f5348"]) as Value
            ),
            Cardano.TransactionOutput.new(
                selfAddress,
                GetHypeNftsOutput(["48595045534B554C4C535F5652545F30303031"]) as Value
            ),
            Cardano.TransactionOutput.new(
                selfAddress,
                GetHypeNftsOutput([
                    "48595045534B554C4C535F56545F53505F4545", 
                    "48595045534B554C4C535F56545F4D4B5F43", 
                    "48595045534B554C4C535F56545F4E554747455453"]) as Value
            )
        ];

        const transactionOutputs = Cardano.TransactionOutputs.new();

        outputs.forEach(output => transactionOutputs.add(output));

        const csResult = CoinSelection.randomImprove(
            utxos.map(utxo => Cardano?.TransactionUnspentOutput.from_bytes(fromHex(utxo)) as TransactionUnspentOutput),
            transactionOutputs,
            8,
            scriptUtxos
        );

        csResult.inputs.forEach((utxo) => {
            txBuilder.add_input(
                utxo.output().address(),
                utxo.input(),
                utxo.output().amount()
            );
        });

        outputs.forEach(output => txBuilder.add_output(output));

        const requiredSigners = Cardano.Ed25519KeyHashes.new();
        requiredSigners.add(baseAddress.payment_cred().to_keyhash() as Ed25519KeyHash);
        txBuilder.set_required_signers(requiredSigners);

        const shDatumObject = VTClaimShDatum() as PlutusDataObject;
        const vrtDatumObject = VTClaimVrtDatum("") as PlutusDataObject;
        const vtDatumObject = VTClaimVtDatum("d3c81d34ac1ebd82c9f7afe1a19b2ccb5200b5e82830de42d8545f251c08c77a") as PlutusDataObject;

        const shDatum = ToPlutusData(shDatumObject) as PlutusData;
        const vrtDatumHash = ToPlutusData(vrtDatumObject) as PlutusData;
        const vtDatumHash = ToPlutusData(vtDatumObject) as PlutusData;

        const datumList = Cardano.PlutusList.new();
        datumList.add(shDatum);
        datumList.add(vrtDatumHash);
        datumList.add(vtDatumHash);

        const redeemers = Cardano.Redeemers.new();
        scriptUtxos.forEach(utxo => {
            const scriptInputIndex = txBuilder.index_of_input(utxo.input());
            redeemers.add(VTClaimPulloutRedeemer(scriptInputIndex) as Redeemer);
        });

        txBuilder.set_plutus_scripts(VTClaimContractScript() as PlutusScripts);
        txBuilder.set_plutus_data(Cardano.PlutusList.from_bytes(datumList.to_bytes()));
        txBuilder.set_redeemers(Cardano.Redeemers.from_bytes(redeemers.to_bytes()));

        transactionWitnessSet.set_plutus_scripts(VTClaimContractScript() as PlutusScripts);
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

        console.log("full tx size", signedTx.to_bytes().length);
        await hsVaporSignalRConnection.send("SubmitVTClaimTx", toHex(signedTx.to_bytes()), []);
    }
}

async function VaporizeBootstrapAsync() {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        setCoinSelectionCardanoSerializationLib(Cardano);
        const protocolParameters = await GetProtocolProtocolParamsAsync();
        CoinSelection.setProtocolParameters(
            protocolParameters.min_utxo.toString(),
            protocolParameters.min_fee_a.toString(),
            protocolParameters.min_fee_b.toString(),
            protocolParameters.max_tx_size.toString()
        );

        if (!await window.cardano.isEnabled()) await window.cardano.enable();
        const txBuilder = await CreateTransactionBuilderAsync() as TransactionBuilder;
        const selfAddress = Cardano.Address.from_bytes(fromHex(await GetWalletAddressAsync()));

        const transactionWitnessSet = Cardano.TransactionWitnessSet.new();
        const shDatumObject = VTClaimShDatum() as PlutusDataObject;
        const vrtDatumObject = VTClaimVrtDatum("") as PlutusDataObject;
        const vtDatumObject = VTClaimVtDatum("d3c81d34ac1ebd82c9f7afe1a19b2ccb5200b5e82830de42d8545f251c08c77a") as PlutusDataObject;

        const shDatumHash = Cardano.hash_plutus_data(ToPlutusData(shDatumObject) as PlutusData);
        const vrtDatumHash = Cardano.hash_plutus_data(ToPlutusData(vrtDatumObject) as PlutusData);
        const vtDatumHash = Cardano.hash_plutus_data(ToPlutusData(vtDatumObject) as PlutusData);

        console.log("shDatumHash", toHex(shDatumHash.to_bytes()));
        console.log("shDatumObject", shDatumObject);

        console.log("vrtDatumHash", toHex(vrtDatumHash.to_bytes()));
        console.log("vrtDatumObject", vrtDatumObject);

        console.log("vtDatumHash", toHex(vtDatumHash.to_bytes()));
        console.log("vtDatumObject", vtDatumObject);

        const shOutput = Cardano.TransactionOutput.new(
            VTClaimContractAddress() as Address,
            GetHypeNftsOutput(["48595045534b554c4c303030315f5348"]) as Value
        );
        shOutput.set_data_hash(shDatumHash);

        const vrtOutput = Cardano.TransactionOutput.new(
            VTClaimContractAddress() as Address,
            GetHypeNftsOutput(["48595045534B554C4C535F5652545F30303031"]) as Value
        );
        vrtOutput.set_data_hash(vrtDatumHash);

        const vtOutput = Cardano.TransactionOutput.new(
            VTClaimContractAddress() as Address,
            GetHypeNftsOutput(["48595045534B554C4C535F56545F53505F4545", "48595045534B554C4C535F56545F4D4B5F43", "48595045534B554C4C535F56545F4E554747455453"]) as Value
        );
        vtOutput.set_data_hash(vtDatumHash);

        const transactionOutputs = Cardano.TransactionOutputs.new();
        transactionOutputs.add(shOutput);
        transactionOutputs.add(vrtOutput);
        transactionOutputs.add(vtOutput);

        const utxos = await window.cardano.getUtxos();
        const csResult = CoinSelection.randomImprove(
            utxos.map(utxo => Cardano?.TransactionUnspentOutput.from_bytes(fromHex(utxo)) as TransactionUnspentOutput),
            transactionOutputs,
            8
        );

        csResult.inputs.forEach((utxo) => {
            txBuilder.add_input(
                utxo.output().address(),
                utxo.input(),
                utxo.output().amount()
            );
        });

        txBuilder.add_output(shOutput);
        txBuilder.add_output(vrtOutput);
        txBuilder.add_output(vtOutput);

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

        console.log("full tx size", signedTx.to_bytes().length);
        await hsVaporSignalRConnection.send("SubmitVTClaimTx", toHex(signedTx.to_bytes()), 
        [shDatumObject,vrtDatumObject,vtDatumObject]);

    }
}

const GetShadowHSData = (name: string) => {
    return shadowHSUtxos
        .find(utxo => utxo.Amounts
            .find((a: any) => a.Unit.toLowerCase() == "bf2c603d38ce68c6d875a097b5e6623fe0f5381d9171e06108e0aec9" + name));
}

const GetHypeNftsOutput = (assetNames: string[]) => {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        let val = Cardano.Value.new(toBigNum(MinUtxoLovelace));
        assetNames.forEach(n => 
            val = val.checked_add(AssetValue(
                toBigNum(0),
                "bf2c603d38ce68c6d875a097b5e6623fe0f5381d9171e06108e0aec9",
                n,
                toBigNum(1)) as Value)
        )
        return val;
    }
}

const VTClaimShDatum = () => {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        const datum = new PlutusDataObject(0);
        datum.Fields = []
        return datum
    }
}

const VTClaimVrtDatum = (pkh: string = "") => {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        const datum = new PlutusDataObject(1);
        datum.Fields = [
            {
                Index: 1,
                Type: PlutusFieldType.Bytes,
                Key: "pkh",
                Value: pkh
            } as PlutusField
        ]
        console.log(datum);
        return datum
    }
}

const VTClaimVtDatum = (hash: string = "") => {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        const datum = new PlutusDataObject(2);
        datum.Fields = [
            {
                Index: 2,
                Type: PlutusFieldType.Bytes,
                Key: "vrtHash",
                Value: hash
            } as PlutusField
        ]
        return datum
    }
}

const VaporizePtDatum = (price: number = 70) => {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        const datum = new PlutusDataObject(1);
        datum.Fields = [
            {
                Index: 0,
                Type: PlutusFieldType.Integer,
                Key: "price",
                Value: price
            } as PlutusField
        ]
        return datum
    }
}

const VaporizeShDatum = (pkh: string = "", orders: string[] = [], delivered: string[]) => {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        const datum = new PlutusDataObject(0);
        datum.Fields = [
            {
                Index: 0,
                Type: PlutusFieldType.Data,
                Key: "vaporizeList",
                Value: VaporizeList(pkh, orders, delivered)
            } as PlutusField,
        ]
        return datum
    }
}

const VaporizeList = (pkh: string = "", orders: string[] = [], delivered: string[]) => {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        const datum = new PlutusDataObject(0);
        datum.Fields = [
            {
                Index: 0,
                Type: PlutusFieldType.Data,
                Key: "pkh",
                Value: pkh
            } as PlutusField,
            {
                Index: 0,
                Type: PlutusFieldType.List,
                Key: "orders",
                Value: orders.map(o => ({
                    Index: 0,
                    Type: PlutusFieldType.Bytes,
                    Key: "vtName",
                    Value: o,
                }))
            } as PlutusField,
            {
                Index: 0,
                Type: PlutusFieldType.Bytes,
                Key: "delivered",
                Value: delivered.map(d => ({
                    Index: 0,
                    Type: PlutusFieldType.Bytes,
                    Key: "vtName",
                    Value: d
                }))
            } as PlutusField,
        ]
        return datum
    }
}

const VTClaimCommitSkullRedeemer = (index: number) => {
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
                Cardano.BigNum.from_str("5000000"),
                Cardano.BigNum.from_str("3000000000")
            )
        )
    }
}

const VTClaimCommitRandomRedeemer = (index: number) => {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        const redeemerData = Cardano.PlutusData.new_constr_plutus_data(
            Cardano.ConstrPlutusData.new(
                Cardano.Int.new_i32(1),
                Cardano.PlutusList.new()
            )
        );

        return Cardano.Redeemer.new(
            Cardano.RedeemerTag.new_spend(),
            toBigNum(index),
            redeemerData,
            Cardano.ExUnits.new(
                Cardano.BigNum.from_str("4000000"),
                Cardano.BigNum.from_str("1200000000")
            )
        )
    }
}

const VTClaimUseRandomRedeemer = (index: number) => {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        const redeemerData = Cardano.PlutusData.new_constr_plutus_data(
            Cardano.ConstrPlutusData.new(
                Cardano.Int.new_i32(2),
                Cardano.PlutusList.new()
            )
        );

        return Cardano.Redeemer.new(
            Cardano.RedeemerTag.new_spend(),
            toBigNum(index),
            redeemerData,
            Cardano.ExUnits.new(
                Cardano.BigNum.from_str("4000000"),
                Cardano.BigNum.from_str("1200000000")
            )
        )
    }
}

const VTClaimProveRedeemer = (index: number) => {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        const redeemerData = Cardano.PlutusData.new_constr_plutus_data(
            Cardano.ConstrPlutusData.new(
                Cardano.Int.new_i32(3),
                Cardano.PlutusList.new()
            )
        );

        return Cardano.Redeemer.new(
            Cardano.RedeemerTag.new_spend(),
            toBigNum(index),
            redeemerData,
            Cardano.ExUnits.new(
                Cardano.BigNum.from_str("4000000"),
                Cardano.BigNum.from_str("1200000000")
            )
        )
    }
}

const VTClaimWithdrawRedeemer = (index: number) => {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        const redeemerData = Cardano.PlutusData.new_constr_plutus_data(
            Cardano.ConstrPlutusData.new(
                Cardano.Int.new_i32(4),
                Cardano.PlutusList.new()
            )
        );

        return Cardano.Redeemer.new(
            Cardano.RedeemerTag.new_spend(),
            toBigNum(index),
            redeemerData,
            Cardano.ExUnits.new(
                Cardano.BigNum.from_str("4000000"),
                Cardano.BigNum.from_str("1200000000")
            )
        )
    }
}

const VTClaimPulloutRedeemer = (index: number) => {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        const redeemerData = Cardano.PlutusData.new_constr_plutus_data(
            Cardano.ConstrPlutusData.new(
                Cardano.Int.new_i32(5),
                Cardano.PlutusList.new()
            )
        );

        return Cardano.Redeemer.new(
            Cardano.RedeemerTag.new_spend(),
            toBigNum(index),
            redeemerData,
            Cardano.ExUnits.new(
                Cardano.BigNum.from_str("2598292"),
                Cardano.BigNum.from_str("855151231")
            )
        )
    }
}


const VTClaimContractScript = () => {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        const scripts = Cardano.PlutusScripts.new();
        scripts.add(Cardano.PlutusScript.new(fromHex(vtClaimContract)));
        return scripts;
    }
};

const VaporizeContractScript = () => {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        const scripts = Cardano.PlutusScripts.new();
        scripts.add(Cardano.PlutusScript.new(fromHex(vaporizeContract)));
        return scripts;
    }
};

const GetOutputFromChainData = (addr: Address, chainData: CardanoChainData, extra: number = 0) => {
    let Cardano = CardanoSerializationLib();
    if (Cardano !== null) {
        let val = Cardano.Value.new(toBigNum(chainData.Amounts.find(a => a.Unit == "lovelace")?.Quantity));
        chainData.Amounts.filter(a => a.Unit != "lovelace")
            .forEach(a => 
                val = val.checked_add(AssetValue(
                    toBigNum(0),
                    a.Unit.slice(0,56),
                    a.Unit.substring(56),
                    toBigNum(a.Quantity)) as Value)
        )

        if(extra >= 2000000)
            val = val.checked_add(Cardano.Value.new(toBigNum(extra)));

        return Cardano.TransactionOutput.new(
            addr,
            val
        )
    }
}

const GetWalletAddressAsync = async () => (await window.cardano.getUsedAddresses())[0];
const toHex = (bytes: Uint8Array) => Buffer.from(bytes).toString("hex");
const asciiToHex = (str: string) => Buffer.from(str, "ascii").toString("hex");
const fromHex = (hex: string) => Buffer.from(hex, "hex");
const GetPkhFromOrder = (order: any) => order.Fields.filter((f: any) => f.Key === "siSeller")[0].Value.toString();
const GetRateFromOrder = (order: any) => order.Fields.filter((f: any) => f.Key === "siRate")[0].Value as number;

const toBigNum = (value: any) => {
    let Cardano = CardanoSerializationLib();
    return Cardano?.BigNum.from_str(value.toString()) as BigNum;
}

document.onreadystatechange = async () => {
    if (document.readyState === "complete") await Main();
};