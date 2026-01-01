import seaborn
import sys
import polars
import matplotlib.pyplot
import numpy


bits = 8


def main():
    with open(sys.argv[1], "rb") as file:
        array = numpy.array(list(file.read()))

    frame2 = polars.DataFrame(
        {
            "code": frame["code"].repeat_by(bits),
            "bit": polars.Series("bit", range(bits)).repeat_by(len(frame["code"])),
        }
    )

    seaborn.displot(frame, x="code", discrete=True)
    seaborn.displot(frame, x="bit", hue="flag", discrete=True)

    matplotlib.pyplot.show()


if __name__ == "__main__":
    main()
