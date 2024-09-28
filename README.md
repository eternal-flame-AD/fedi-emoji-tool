# Fedi-Emoji-Tool

A tool to fetch and pack custom emojis from federated Misskey and Mastodon instances for Misskey import.

The tools is designed to be efficient and polite to other instances. Notably:

- Concurrent connections and request frequency are limited on a per-instance basis.
- The tool will not fetch the same emoji twice, and will shuffle the order of fetching to avoid spamming the same instance.
- The tool will not fetch emojis that any instance has marked with a license flag.
- The tool will not fetch emojis that are marked as sensitive.
- The tool will not fetch emojis that are present in very few instances (default threshold 5), to avoid fetching original content that is not meant to be shared.

## Expectations

On [my instance](https://mi.yumechi.jp) with 6.6k federations, the tool selected ~47k unique emojis under the default filter.

My memory optimization goal is <8GB usage for a full pack of 47k emojis.

## Usage 

### Quick Start

#### Preconditions

- Nothing on the regular workflow need authentication, but to actually import the packed ZIP file, an Misskey-like instance that you have permission to upload custom emojis.
- An instance that has a good amount of federations with other instances as the "home server", technically do not have to be the same instance as the above. ~1000 is a good number to start with. I intentionally do not support recursive fetching to avoid spamming the instances and fetching into unwanted territories. Banned instances are implicitly excluded.
- There is a pre-built docker container (`docker.io/l1drm/fedi-emoji-tool`), or you need to build the tool from source, [`ghcup`](https://www.haskell.org/ghcup/#) is recommended for setting up Haskell development environment, a `cabal build` should be enough to build the tool.

#### Steps

- Do a quick sanity test by running the `home-emojis` and `instances` commands to see if the tool can connect to the instance and fetch the emoji and federation data.
- Run `fetch-remote-emojis` to fetch the remote emojis from the instances, this will generate a JSON file that describes the unique emojis and the instances that have them. It would look like this, it is designed to be as flat as possible to ease `jq`-based filtering and manual editing:

```json
{
  {
    "name": "ameowtrampoline",
    "seenAt": [
      {
        "aliases": [],
        "category": "Meow Moji Animated",
        "isSensitive": null,
        "localOnly": null,
        "name": "ameowtrampoline",
        "url": "https://pika.moe/files/de2f0cf0-4cc0-4131-96cb-21038b602452"
      },
      {
        "aliases": [],
        "category": "03_blobcat/A",
        "isSensitive": null,
        "localOnly": null,
        "name": "ameowtrampoline",
        "url": "https://mogeko.monster/files/8c7b4769-021e-45ff-b977-e8977e970d28"
      }
    ]
  },
}
```

- Run `pack-remote-emojis` to download emojis that fit the criteria into a directory and generate a misskey-compatible metadata JSON for importing. The name and alias and category of the emoji will be deduced by the majority of the instances that have them. Output will be chunked into 2048-emoji subdirectories because misskey cannot read large zip files.
- Use your favorite ZIP tool to pack the directory into a ZIP file, and import it into your Misskey-like instance.
- When you want to update, there are `meta-intersect` and `meta-merge` commands to help you to manage the metadata JSON files.

### Full CLI Usage

```bash
fedi-emoji-tool - A tool to fetch and pack custom emojis from Misskey and
Mastodon instances for Misskey

Usage: fedi-emoji-tool [-h|--home URL] [-l|--pagination INT] [-t|--timeout INT] 
                       [--head INT] [-v|--verbose] COMMAND

  A tool to fetch and pack custom emojis from Misskey and Mastodon instances

Available options:
  -h,--home URL            Home URL of the Misskey instance, will read
                           MISSKEY_HOME environment variable if not provided
  -l,--pagination INT      Pagination limit
  -t,--timeout INT         HTTP timeout
  --head INT               Only process the first N entries as a test run
  -v,--verbose             Verbose output

Available commands:
  home-emojis              List custom emojis at home, sanity check mainly
  instances                List federation instances at home, sanity check
                           mainly
  fetch-remote-emojis      Fetch remote non-licensed emojis into a JSON file
                           describing unique emojis and instances with them
  meta-intersect           Intersect or complement two meta emoji files
  meta-merge               Merge multiple meta emoji files
  pack-remote-emojis       Pack the collected remote emojis into a directory
                           that can be ZIPped and imported into Misskey-like
                           instances
  clear-local-only-emojis  [Auth required] Clear local-only emojis, a mistake
                           fixer

Author: Yume <@yume@mi.yumechi.jp>
```

```bash
Usage: fedi-emoji-tool fetch-remote-emojis (-o|--output OUTPUT)

  Fetch remote non-licensed emojis into a JSON file describing unique emojis and
  instances with them

Available options:
  -o,--output OUTPUT       Output file to save remote emojis

Global options:
  -h,--home URL            Home URL of the Misskey instance, will read
                           MISSKEY_HOME environment variable if not provided
  -l,--pagination INT      Pagination limit
  -t,--timeout INT         HTTP timeout
  --head INT               Only process the first N entries as a test run
  -v,--verbose             Verbose output
```

```bash
Usage: fedi-emoji-tool pack-remote-emojis 
         (-i|--input INPUT) (-o|--output-dir DIR) [-d|--delay DELAY] 
         [-m|--min-count MIN_COUNT] [-s|--size-limit SIZE_LIMIT]

  Pack the collected remote emojis into a directory that can be ZIPped and
  imported into Misskey-like instances

Available options:
  -i,--input INPUT         Input file to upload remote emojis
  -o,--output-dir DIR      Working directory for the emojis
  -d,--delay DELAY         Delay between requests to the same host in seconds,
                           default 0.66
  -m,--min-count MIN_COUNT Minimum count of instances to use an emoji for it to
                           be included, default 5
  -s,--size-limit SIZE_LIMIT
                           Size limit for downloaded files in MiB, default 32

Global options:
  -h,--home URL            Home URL of the Misskey instance, will read
                           MISSKEY_HOME environment variable if not provided
  -l,--pagination INT      Pagination limit
  -t,--timeout INT         HTTP timeout
  --head INT               Only process the first N entries as a test run
  -v,--verbose             Verbose output
```