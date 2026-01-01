import seaborn
import sys
import polars
import matplotlib.pyplot


def main():
    frame = polars.read_csv(sys.argv[1])

    seaborn.displot(frame, x="offset", discrete=True)
    seaborn.displot(frame, x="length", discrete=True)

    matplotlib.pyplot.show()


if __name__ == "__main__":
    main()
