Feature: Main
  Scenario Outline: Escape a backslash
    Then "\\" is "<value>"

    Examples:
      | value |
      | \\\\  |

  Scenario Outline: Escape a newline
    Then "\n" is "<value>"

    Examples:
      | value |
      | \n    |
      | \\n   |

  Scenario Outline: Escape a vertical bar
    Then "|" is "<value>"

    Examples:
      | value |
      | \|    |
