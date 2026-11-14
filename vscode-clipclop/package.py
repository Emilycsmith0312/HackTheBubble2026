#!/usr/bin/env python3
"""Build the Clip Clop VS Code extension package (a .vsix) without node or vsce.

A .vsix is a zip file holding the extension under extension/, plus two
manifest files describing it. Run from anywhere:  python3 package.py
"""
import json
import os
import zipfile
from xml.sax.saxutils import escape

HERE = os.path.dirname(os.path.abspath(__file__))

# Everything that goes in the package, relative to this directory.
FILES = [
    "package.json",
    "README.md",
    "extension.js",
    "language-configuration.json",
    "syntaxes/clipclop.tmLanguage.json",
    "snippets/clipclop.json",
    "icons/dotclipclop.svg",
]

CONTENT_TYPES = """<?xml version="1.0" encoding="utf-8"?>
<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">
  <Default Extension=".json" ContentType="application/json"/>
  <Default Extension=".js" ContentType="application/javascript"/>
  <Default Extension=".md" ContentType="text/markdown"/>
  <Default Extension=".svg" ContentType="image/svg+xml"/>
  <Default Extension=".vsixmanifest" ContentType="text/xml"/>
</Types>
"""

MANIFEST = """<?xml version="1.0" encoding="utf-8"?>
<PackageManifest Version="2.0.0" xmlns="http://schemas.microsoft.com/developer/vsx-schema/2011">
  <Metadata>
    <Identity Language="en-US" Id="{name}" Version="{version}" Publisher="{publisher}"/>
    <DisplayName>{displayName}</DisplayName>
    <Description xml:space="preserve">{description}</Description>
    <Categories>{categories}</Categories>
    <Properties>
      <Property Id="Microsoft.VisualStudio.Code.Engine" Value="{engine}"/>
      <Property Id="Microsoft.VisualStudio.Code.ExtensionKind" Value="workspace"/>
    </Properties>
  </Metadata>
  <Installation>
    <InstallationTarget Id="Microsoft.VisualStudio.Code"/>
  </Installation>
  <Dependencies/>
  <Assets>
    <Asset Type="Microsoft.VisualStudio.Code.Manifest" Path="extension/package.json" Addressable="true"/>
    <Asset Type="Microsoft.VisualStudio.Services.Content.Details" Path="extension/README.md" Addressable="true"/>
  </Assets>
</PackageManifest>
"""


def main():
    with open(os.path.join(HERE, "package.json")) as f:
        pkg = json.load(f)

    manifest = MANIFEST.format(
        name=escape(pkg["name"]),
        version=escape(pkg["version"]),
        publisher=escape(pkg["publisher"]),
        displayName=escape(pkg["displayName"]),
        description=escape(pkg["description"]),
        categories=escape(",".join(pkg.get("categories", []))),
        engine=escape(pkg["engines"]["vscode"]),
    )

    out = os.path.join(HERE, "{}-{}.vsix".format(pkg["name"], pkg["version"]))
    with zipfile.ZipFile(out, "w", zipfile.ZIP_DEFLATED) as z:
        z.writestr("[Content_Types].xml", CONTENT_TYPES)
        z.writestr("extension.vsixmanifest", manifest)
        for name in FILES:
            z.write(os.path.join(HERE, name), "extension/" + name)
    print("wrote " + out)


if __name__ == "__main__":
    main()
