import sys
import json
import prop


def main():
    json.dump(
        {
            "c": prop.c(),
            "c20": prop.c(20_000),
            "j": prop.j(),
        },
        sys.stdout,
        indent=2,
        sort_keys=True,
    )


if __name__ == "__main__":
    main()
