import seaborn
import sys
import matplotlib.pyplot
import numpy


bits = 8


def main():
    with open(sys.argv[1], "rb") as file:
        array = numpy.array(list(file.read()))

    seaborn.displot(array, x=0, discrete=True)
    # seaborn.displot(array, x=0, hue=2, discrete=True)

    matplotlib.pyplot.show()


if __name__ == "__main__":
    main()
