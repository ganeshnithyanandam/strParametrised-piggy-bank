# TODO Update readme
# Parametrised Piggy Bank

This repo houses a simple Plutus Contract that acts as a parametrised shared piggy bank.

## Setting up

### Cabal+Nix build

Set up your machine to build things with `Nix`, following the [Plutus README](https://github.com/input-output-hk/plutus/blob/master/README.adoc) (make sure to set up the binary cache!).

To enter a development environment, simply open a terminal on the project's root and use `nix-shell` to get a bash shell:

```
$ nix-shell
```

Otherwise, you can use [direnv](https://github.com/direnv/direnv) which allows you to use your preferred shell. Once installed, just run:

```
$ echo "use nix" > .envrc # Or manually add "use nix" in .envrc if you already have one
$ direnv allow
```

and you'll have a working development environment for now and the future whenever you enter this directory.

The build should not take too long if you correctly set up the binary cache. If it starts building GHC, stop and setup the binary cache.

Afterwards, the command `cabal build server` from the terminal should work (if `cabal` couldn't resolve the dependencies, run `cabal update` and then `cabal build`).

## The Plutus Application Backend (PAB)

With the PAB we can serve and interact with contracts over a web API.
You can read more about the PAB here: [PAB Architecture](https://github.com/input-output-hk/plutus/blob/master/plutus-pab/ARCHITECTURE.adoc).

Here, the PAB is configured with one contract, the `ParametrisedPiggyBank` contract from `./src/Plutus/Contracts/ParametrisedPiggyBank.hs`.

Here's an example of running and interacting with this contract via the API. For this it will help if you have `jq` installed.

1. Compile the contract and build the server:

```
cabal build str-ppb
```

2. Run the PAB binary:

```
cabal exec -- str-ppb
````

This will then start up the server on port 9080.

3. Check what contracts are present:

```
curl -s http://localhost:9080/api/contract/definitions | jq
```

You should receive a list of contracts and the endpoints that can be called on them, and the arguments
required for those endpoints.

We're interested in the `StrParameterisedPiggyBankContract`.

## Run the PAB executable
### Parametrised piggy banks
These piggy banks are parametrised by the name of the beneficiary. In practice the public key hash of the beneficiary wallet will be used for parametrisation. Here in order to avoid complexity of passing in pkh in the REST api requests and convering it to a PublicKeyHash, a simple string is used. This still demostrates the use of parametrisation.

### Put, empty, inspect and logPkh

After starting the PAB, you can run `./run.sh` to send requests to the server and observe the results.

First, we activate four distinct wallets. Dad, Mom, Jack and Jill. 
Each script address to which lovelace can be sent is a piggy bank.

Then in order this following requests and responses can be observed

- Using the `logPkh` endpoint the pkh of Jack's and Jill's wallet are printed.
- Dad adds 2000000 lovelace to Jack's piggy bank using `put` endpoint.
- Mom then puts 3000000 lovelave to Jill's piggy bank.
- Dad uses `inspect` endpoint and sees that Jack's piggy bank now has the lovelace he put.
- Jack empties his piggy bank using the `empty` endpoint.
- `inspect` Jack's piggy bank and can be seen to be empty
- Finally Jill empties her piggy bank using the `empty` endpoint.


Note that:
It was possible to create two different script addresses for Jack and Jill to be used as piggy bank.
