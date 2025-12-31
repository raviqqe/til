import seaborn
import sys


def main():
    compression = seaborn.load_dataset(sys.argv[1])

    seaborn.displot(compression, x="offset")
    seaborn.displot(compression, x="length")


if __name__ == "__main__":
    main()
