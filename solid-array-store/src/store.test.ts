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

it("reconciles an object with an array", () => {
  const [store, setStore] = createStore<{ value: {} | [] }>({
    value: { foo: "bar" },
  });

  setStore("value", reconcile([0, 1, 2]));

  expect(Array.isArray(store.value)).toBe(true);
});

it("swaps an array with an object", () => {
  const [store, setStore] = createStore<{ value: {} | [] }>({
    value: [0, 1, 2],
  });

  setStore("value", reconcile({ foo: "bar" }));

  expect(Array.isArray(store.value)).toBe(false);
});
