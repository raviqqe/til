import seaborn
import sys
import polars
import matplotlib.pyplot


def main():
    with open(sys.argv[1], "rb") as file:
        frame = polars.DataFrame({"code": list(file.read())})

    seaborn.displot(frame, x="code", discrete=True)
    seaborn.displot(frame, x="bit", hue="flag", discrete=True)

    matplotlib.pyplot.show()


if __name__ == "__main__":
    main()
