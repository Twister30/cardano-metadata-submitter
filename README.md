# Cardano Metadata Submitter

TODO: Get real build badge
[![Build status](https://badge.buildkite.com/e5b12d0fd507084fbdb1849da2de467f1de66b3e5c6d954554.svg)](https://buildkite.com/input-output-hk/iohk-nix)

A library and CLI for manipulating data intended for the [Goguen Metadata Registry](https://github.com/cardano-foundation/goguen-metadata-registry). See the link for more information about the Metadata Registry itself.

## Usage

For both building with `nix-build` and using `nix-shell`, it might take a very long
time if you do not have the Cardano binary cache set up. Adding the
IOHK binary cache to your Nix configuration will speed up builds a lot,
since many things will have been built already by our CI.

If you find you are building packages that are not defined in this
repository, or if the build seems to take a very long time then you may
not have this set up properly.

To set up the cache:

* On non-NixOS, edit /etc/nix/nix.conf and add the following lines:

```
substituters        = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
```

* On NixOS, set the following NixOS options:

```
nix = {
  binaryCaches          = [ "https://hydra.iohk.io" "https://iohk.cachix.org" ];
  binaryCachePublicKeys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo=" ];
};
```

### Building

`nix-build` will build all the library components, including test suite.


### Shell

To get a shell which has the tool in scope you can use `nix-shell -A devops`.

### Accessing the registry

This tool is used in the context of editing a working copy of the [Goguen Metadata Registry](https://github.com/cardano-foundation/goguen-metadata-registry), and so in order to use it, you must first make a checkout of it, and
change your working directory to the `registry` subdirectory of it:

```bash
git checkout git@github.com:cardano-foundation/goguen-metadata-registry
cd goguen-metadata-registry/registry
```

### Creating a new entry or modifying an preexisting one:

Data is submitted to the Goguen Metadata Registry via pull request of a single file. The name of the file corresponds to the subject of the metadata entry. This CLI tool has been designed to fit into a git based workflow for creating and modifying entries.

Suppose we have a hash of an asset whose semantics we want to publish to the metadata repository. For example, this could be the hash of a smart contract we would like to document for the benefit of users, as Goguen scripts can only be invoked by providing the contents corresponding to their hash in the transactions that execute them.  To create a new metadata entry, while working in a checkout of the metadata repository, we can run the following command:

```
goguen-metadata-repository> cardano-metadata-submitter -i 3513560a0f272e96605cd88c0c892208e00781ba2403c1127c7b1da34fdbf058
```

The result of running this query is a new file in the working directory, `3513560a0f272e96605cd88c0c892208e00781ba2403c1127c7b1da34fdbf058.json.draft`. Note the `.draft` extension which signals that we are still working on this piece of metadata. The contents of the file is as follows:

```
{
    "subject": "3513560a0f272e96605cd88c0c892208e00781ba2403c1127c7b1da34fdbf058"
}
```

If you pass a subject that already has an entry to `-i`, then your draft will be a freshly initialized version. To modify
the original instead, do not provide `-i`, and your draft will be a copy of the original.

### Adding a preimage

The most important piece of metadata for our script hash is, of course, the script itself. We can specify a preimage for our subject as such:

```
cardano-metadata-submitter 3513560a0f272e96605cd88c0c892208e00781ba2403c1127c7b1da34fdbf058 -H sha256 -p '6d792d676f6775656e2d736372697074'
```

The result is:


```
{
    "subject": "3513560a0f272e96605cd88c0c892208e00781ba2403c1127c7b1da34fdbf058",
    "preImage": {
        "value": "6d792d676f6775656e2d736372697074",
        "hashFn": "sha256"
    }
}
```

Note that the preimage is a byte string! If your subject is the hash of a utf-8 encoded string, you can convert it to bytes with `xxd`:

```
> echo -n 'my-goguen-script' | xxd -p
6d792d676f6775656e2d736372697074

```

#### Adding a token name to a subject

`preimage` is an optional field. One situation where preimage is not specified is where the subject is,
in fact, a hash of a script plus a specific token name. This can be specified on the command line by using
the script name as the usual subject argument, plus an additional `-t` flag to specify the token name.
The token name will be encoded as hex and appended to the subject:

```
> cardano-metadata-submitter -i 3fec5769b5cf4e310a7d150508e82fb8e3eda1c2c94c61492d3bd8aea99e06c9 -t 'MySpecialToken'
3fec5769b5cf4e310a7d150508e82fb8e3eda1c2c94c61492d3bd8aea99e06c94d795370656369616c546f6b656e.json.draft
```

For these situations, the longer subject can also be specified directly. This is merely a convenience for
calculating the subject in these situations:

```
> cardano-metadata-submitter -i 3fec5769b5cf4e310a7d150508e82fb8e3eda1c2c94c61492d3bd8aea99e06c94d795370656369616c546f6b656e
3fec5769b5cf4e310a7d150508e82fb8e3eda1c2c94c61492d3bd8aea99e06c94d795370656369616c546f6b656e.json.draft
```

No preimage will be allowed (as it will not hash to the longer subject).

### Adding multiple fields at once

We can specify multiple pieces of metadata at once. Here we will add a name and description for our script hash in one invocation:

```
cardano-metadata-submitter 3513560a0f272e96605cd88c0c892208e00781ba2403c1127c7b1da34fdbf058 -n "My Goguen Script" -d "A script I have registered on chain"
```


```
{
    "subject": "3513560a0f272e96605cd88c0c892208e00781ba2403c1127c7b1da34fdbf058",
    "preImage": {
        "value": "6d792d676f6775656e2d736372697074",
        "hashFn": "sha256"
    },
    "name": {
        "value": "My Goguen Script",
        "anSignatures": [
        ]
    },
    "description": {
        "value": "A script I have registered on chain",
        "anSignatures": [
        ]
    }
}
```

#### Adding a logo field

To add a logo field, specify `-l` and a file containing the logo, which must be in PNG format.
It will be encoded in base64 in the `.json` file.

```
> cardano-metadata-submitter 3513560a0f272e96605cd88c0c892208e00781ba2403c1127c7b1da34fdbf058 -l logo.png
```

Currently, ownership signatures are not supported if a logo is present.

### Adding attestation signatures to entries

Anyone can submit metadata to the repository. While the metadata is verified by humans before inclusion, it is desirable that there be a way to attest to the validity of the metadata with a signature. This can be done with an ED25519 key. You can use a signing key to attest to all fields present in an entry as follows:

```
cardano-metadata-submitter 3513560a0f272e96605cd88c0c892208e00781ba2403c1127c7b1da34fdbf058 -a attestation-key.prv
```

```
{
    "subject": "3513560a0f272e96605cd88c0c892208e00781ba2403c1127c7b1da34fdbf058",
    "preImage": {
        "value": "6d792d676f6775656e2d736372697074",
        "hashFn": "sha256"
    },
    "name": {
        "value": "My Goguen Script",
        "anSignatures": [
            {
                "publicKey": "681c37ef6d04ee57e45e15b035ed06bb2110adff93a6e35993bdab1009a576d6",
                "signature": "d366c0a80a9fb8e7fe0bdc710be5551621aeb0b1a75833415b79ddd8b7f85227bbd540df4991440d8a6d7fccb4d3bd739755b4a71659cd8c1059b9699622ff04"
            }
        ]
    },
    "description": {
        "value": "A script I have registered on chain",
        "anSignatures": [
            {
                "publicKey": "681c37ef6d04ee57e45e15b035ed06bb2110adff93a6e35993bdab1009a576d6",
                "signature": "f235875f996fb452d16465ff3af984ece700a6d7bd18569197cf1c2884dc6e0ab74a1a7887145d8c01c0d54fd6a6c57636bdcc1980bf024c42c42e3d7ff3d409"
            }
        ]
    }
}
```

Note that if you modify fields that already have attestation signatures associated with them, those attestation signatures will be removed as they are no longer valid.

To only attest to one field, and not both, you can use flags. `-N` attests just the name, and `-D` attests just
the description. By default, both fields are attested if you provide an attestation key.

### Adding an ownership signature

Any number of signatures can be provided for each field in an entry, allowing multiple identities to attest to the validity of each metadata field for a given subject. The Goguen Metadata Registry also uses ED25519 signatures to verify ownership of a particular entry. Ownership is claimed by signing the entire entry, and only one ownership signature can be provided. You can sign for ownership as follows:

```
cardano-metadata-submitter 3513560a0f272e96605cd88c0c892208e00781ba2403c1127c7b1da34fdbf058 -o owner-key.prv
```

```
{
    "subject": "3513560a0f272e96605cd88c0c892208e00781ba2403c1127c7b1da34fdbf058",
    "preImage": {
        "value": "6d792d676f6775656e2d736372697074",
        "hashFn": "sha256"
    },
    "name": {
        "value": "My Goguen Script",
        "anSignatures": [
            {
                "publicKey": "681c37ef6d04ee57e45e15b035ed06bb2110adff93a6e35993bdab1009a576d6",
                "signature": "d366c0a80a9fb8e7fe0bdc710be5551621aeb0b1a75833415b79ddd8b7f85227bbd540df4991440d8a6d7fccb4d3bd739755b4a71659cd8c1059b9699622ff04"
            }
        ]
    },
    "description": {
        "value": "A script I have registered on chain",
        "anSignatures": [
            {
                "publicKey": "681c37ef6d04ee57e45e15b035ed06bb2110adff93a6e35993bdab1009a576d6",
                "signature": "f235875f996fb452d16465ff3af984ece700a6d7bd18569197cf1c2884dc6e0ab74a1a7887145d8c01c0d54fd6a6c57636bdcc1980bf024c42c42e3d7ff3d409"
            }
        ]
    },
    "owner": {
        "publicKey": "fc5d7e9347fe1cb728815c977d609f3cd51831fc40f22d37f15ec14e20196351",
        "signature": "4866b512189d0fc8023326dcfde4ca104e9c14327d8c20f908289a3a47f4bdc19ba775734d5e50680e9fb305eb19a78fb5a5d8e50f5d70f7b2a81b00957dc203"
    }
}
```

As with attestation signatures, if you modify your entry at all after signing it for ownership, the ownership signature will be removed as it is invalid.

### Creating your PR branch

At this point we have added all of the metadata values we want to our entry, have attested to the validity of each field, and signed the entry with our ownership key. We are ready to finalize the entry and turn it into a pull request to the metadata repository, which also performs validation to ensure that all signatures are valid and all required fields are present:

```
cardano-metadata-submitter 3513560a0f272e96605cd88c0c892208e00781ba2403c1127c7b1da34fdbf058 -f
```

If you haven't been performing these operations in a checkout of your fork of the metadata repository, move your entry there now into the `registry` directory and you can perform the following steps to submit your entry:

```
git checkout -b "my-metadata-submission"
git add registry/3513560a0f272e96605cd88c0c892208e00781ba2403c1127c7b1da34fdbf058.json
git commit -m "Example submission"
git push
```

At this point you can use Github to make your pull request. Remember to check CI and any comments made on your PR before its approval.

### Key Generation

As a convenience, cardano-metadata-submitter can generate an ED25519 keypair for you:

```
cardano-metadata-submitter -K attestation-key
```

This will create two files, `attestation-key.prv` and `attestation-key.pub`. Remember not to include these in your git commits, and store your private key with as much care as any other private key!

### Environment Variable for Current Metadata Subject

If you set the `METADATA_SUBJECT` environnment variable, all commands will assume that as
your current metadata subject if none is provided. This is useful for scripts.

That means that these two snippets are equivalent:
```bash
export METADATA_SUBJECT=3513560a0f272e96605cd88c0c892208e00781ba2403c1127c7b1da34fdbf058
cardano-metadata-submitter -f
```

And:
```bash
cardano-metadata-submitter -f 3513560a0f272e96605cd88c0c892208e00781ba2403c1127c7b1da34fdbf058
```
