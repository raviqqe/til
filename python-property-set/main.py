from pprint import pprint
from property_set import c, j


def main():
    pprint(
        {
            "c": c(),
            "c15": c(15_000),
            "j": j(),
        }
    )


if __name__ == "__main__":
    main()
