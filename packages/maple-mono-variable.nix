{ stdenvNoCC, lib }:
stdenvNoCC.mkDerivation {
  pname = "maple-mono-variable";
  version = "1.0";

  # Vendored fonts in the repository for portability
  src = ../assets/fonts/MapleMono-Variable-patched;

  installPhase = ''
    mkdir -p $out/share/fonts/truetype
    # Copy all TTF/OTF; brace fallback for shells if brace expansion fails
    cp -v $src/*.{ttf,otf} $out/share/fonts/truetype/ || cp -v $src/*.ttf $out/share/fonts/truetype/ || true
  '';

  meta = with lib; {
    description = "Maple Mono Variable (patched) fonts";
    homepage = "https://github.com/subframe7536/maple-font";
    license = licenses.ofl;
    platforms = platforms.all;
  };
}
