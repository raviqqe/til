import sys
import json
import property


def main():
    json.dump(
        {
            "c": property.c(),
            "c15": property.c(15_000),
            "j": property.j(),
        },
        sys.stdout,
        indent=2,
        sort_keys=True,
    )


if __name__ == "__main__":
    main()
