# Fedi-Emoji-Tool

A tool to fetch and pack custom emojis from federated Misskey instances.

The tools is designed to be efficient and polite to other instances. Notably:

- Concurrent connections and request frequency are limited on a per-instance basis.
- The tool will not fetch the same emoji twice, and will shuffle the order of fetching to avoid spamming the same instance.
- The tool will not fetch emojis that any instance has marked with a license or local-only flag.
- The tool will not fetch emojis that are marked as sensitive.
- The tool will not fetch emojis that are present in very few instances (default threshold 5), to avoid fetching original content that is not meant to be shared.

## Expectations

On [my instance](https://mi.yumechi.jp) with 6.6k federations, the tool selected ~12k unique emojis.

## Usage 

### Quick Start

#### Preconditions

- An Misskey-like instance that you either own or have permission to upload custom emojis.
- An instance that has a good amount of federations with other instances, technically do not have to be the same instance as the above. ~1000 is a good number to start with. I intentionally do not support recursive fetching to avoid spamming the instances and fetching into unwanted territories.
- There is a pre-built docker container (`docker.io/l1drm/fedi-emoji-tool`), or you need to build the tool from source, [`ghcup`](https://www.haskell.org/ghcup/#) is recommended for setting up Haskell development environment, a `cabal build` should be enough to build the tool.

#### Steps

- Do a quick sanity test by running the `home-emojis` and `instances` commands to see if the tool can connect to the instance and fetch the emoji and federation data.
- Run `fetch-remote-emojis` to fetch the remote emojis from the instances, this will generate a JSON file that describes the unique emojis and the instances that have them. It would look like this:

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
fedi-emoji-tool - A tool to fetch and pack custom emojis from Misskey instances

Usage: fedi-emoji-tool [-h|--home URL] [-l|--pagination INT] [-t|--timeout INT] 
                       [--head INT] [-v|--verbose] COMMAND

  A tool to fetch and pack custom emojis from Misskey (and pending Mastodon)
  instances

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
  clear-local-only-emojis  Clear local-only emojis, a mistake fixer

Author: Yume <@yume@mi.yumechi.jp>
```