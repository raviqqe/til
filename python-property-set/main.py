from sys import stdout
import json
from property_set import c, j


def main():
    json.dump(
        {
            "c": c(),
            "c15": c(15_000),
            "j": j(),
        },
        stdout,
        indent=2,
        sort_keys=True,
    )


if __name__ == "__main__":
    main()
