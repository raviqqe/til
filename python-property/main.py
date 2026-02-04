import sys
import json
import prop


def main():
    json.dump(
        {
            "c": prop.c(),
            "c10": prop.c(10_000),
            "j": prop.j(),
        },
        sys.stdout,
        indent=2,
        sort_keys=True,
    )


if __name__ == "__main__":
    main()
