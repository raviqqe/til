import seaborn
import sys
import polars
import matplotlib.pyplot


def main():
    compression = polars.read_csv(sys.argv[1])

    seaborn.displot(compression, x="offset", discrete=True)
    seaborn.displot(compression, x="length", discrete=True)

    matplotlib.pyplot.show()


if __name__ == "__main__":
    main()
