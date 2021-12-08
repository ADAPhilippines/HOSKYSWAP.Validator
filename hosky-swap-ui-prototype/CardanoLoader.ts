class CardanoLoader {
    private Loaded: typeof import("@emurgo/cardano-serialization-lib-browser/cardano_serialization_lib") | null = null;

    public async LoadAsync() {
        if (this.Loaded == null) {
            let loaded = await import(
                "@emurgo/cardano-serialization-lib-browser/cardano_serialization_lib"
            );
            this.Loaded = await loaded.default;
        }
        return this.Loaded;
    }

    public CardanoSerializationLib = () => this.Loaded;
}

export default new CardanoLoader();