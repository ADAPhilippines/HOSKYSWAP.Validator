import { TransactionUnspentOutput, BigNum, Vkeywitnesses } from '@emurgo/cardano-serialization-lib-browser';
import CardanoLoader from './CardanoLoader';

const btnSelector = "btnBuy";
const CardanoSerializationLib = CardanoLoader.CardanoSerializationLib;

async function Main() {
    await CardanoLoader.LoadAsync();
    const btnContract = document.getElementById(btnSelector) as HTMLButtonElement;
    btnContract.addEventListener("click", ExecuteContract);
}


async function ExecuteContract() {
    try {
        let Cardano = CardanoSerializationLib();
        console.log("Cardano: ", Cardano);
    } catch(e) {
        console.log("Error: ", e);
    }
}

document.onreadystatechange = async () => {
    if (document.readyState == "complete") await Main();
};