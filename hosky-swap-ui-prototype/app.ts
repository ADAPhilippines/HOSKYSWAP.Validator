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
        if(Cardano !== null) {
            let walletAddresses = await window.cardano.getUsedAddresses();
            // let walletAddressObj = Cardano.Address.from_bytes(fromHex(walletAddresses[0]));
            let walletAddressObj = Cardano.Address.from_bech32("addr_test1qz29cqgwc8ql3p8dw7xz3v7xgnwv9ksu8vwlf2gfynx9rh4e8v6w26ecdktzucd8zsgnyf52usqv4jc8ltatj26lmz9slxy5dh");
            let walletBaseAddressObj = Cardano.BaseAddress.from_address(walletAddressObj);
            console.log(toHex(walletBaseAddressObj?.payment_cred().to_keyhash()?.to_bytes() as Uint8Array));
        }
    } catch(e) {
        console.log("Error: ", e);
    }
}

export const toHex = (bytes: Uint8Array) => Buffer.from(bytes).toString("hex");
export const fromHex = (hex: string) => Buffer.from(hex, "hex");

document.onreadystatechange = async () => {
    if (document.readyState == "complete") await Main();
};