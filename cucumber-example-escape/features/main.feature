Feature: Main
  Scenario Outline: Escape a backslash
    Then "\\" is "<value>"

    Examples:
      | value    |
      | \\\\\\   |
      | \\\\\\\\ |

  Scenario Outline: Escape a newline
    Then "<value>" is a newline

    Examples:
      | value |
      | \n    |

  Scenario Outline: Escape a newline's backslash
    Then "\n" is "<value>"

    Examples:
      | value |
      | \\n   |

  Scenario Outline: Escape a vertical bar
    Then "|" is "<value>"

    Examples:
      | value |
      | \|    |
