{ reflex-platform, ... }: reflex-platform.ghcjs.override {
  overrides = self: super: {
    reflex-dom-contrib = self.callPackage deps/reflex-dom-contrib {};
  };
}
