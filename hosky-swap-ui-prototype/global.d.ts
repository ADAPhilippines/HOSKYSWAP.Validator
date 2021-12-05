import CardanoDAPPConnector from "./Types/CardanoDAppConnector";

declare global {
    interface Window {
        cardano: CardanoDAPPConnector;
    }
}