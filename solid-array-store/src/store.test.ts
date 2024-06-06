import { createStore, reconcile } from "solid-js/store";
import { expect, it } from "vitest";

it("swaps an object with an array", () => {
  const [store, setStore] = createStore<{ value: {} | [] }>({
    value: {},
  });

  setStore(reconcile({ value: [] }));

  expect(Array.isArray(store.value)).toBe(true);
});

it("swaps an array with an object", () => {
  const [store, setStore] = createStore<{ value: {} | [] }>({
    value: [],
  });

  setStore(reconcile({ value: {} }));

  expect(Array.isArray(store.value)).toBe(false);
});
