import seaborn
import sys
import pandas
import matplotlib.pyplot


def main():
    compression = pandas.read_csv(sys.argv[1])

    seaborn.displot(compression, x="offset")
    seaborn.displot(compression, x="length")

    matplotlib.pyplot.show()


if __name__ == "__main__":
    main()
