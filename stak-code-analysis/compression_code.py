import seaborn
import sys
import polars
import matplotlib.pyplot


def main():
    compression = polars.read_csv(sys.argv[1])

    seaborn.displot(compression, x="offset")
    seaborn.displot(compression, x="length")

    matplotlib.pyplot.show()


if __name__ == "__main__":
    main()
