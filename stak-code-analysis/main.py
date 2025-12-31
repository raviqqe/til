import seaborn
import sys
import pandas


def main():
    compression = pandas.read_csv(sys.argv[1])

    seaborn.displot(compression, x="offset")
    seaborn.displot(compression, x="length")


if __name__ == "__main__":
    main()
