Can you integrate the `muffy-validation` crate into the `muffy` crate to enable the HTML validation in the `muffy` crate as well as the existing link validation?

- For document validation in the `muffy` crate, we need to validate each element with the `validate_element` function from the `muffy-validation` crate.
  - This will ensure that all HTML elements in the document are properly validated according to the rules defined in the `muffy-validation` crate.
- The concept of elements in the `muffy-validation` crate directly maps to the one in the `muffy` crate.
- The concept of attributes and children in the `muffy-validation` crate maps to the concept of _items_ in the `muffy` crate.
  - For example, when a URL in an `href` attribute of a `div` element is dead, we should emit two errors for the _item_ of the `href` attribute: one for the dead URL and the other for the invalid `div` element containing the `href` attribute.
