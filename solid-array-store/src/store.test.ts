import { createStore, flush, reconcile } from "solid-js";
import { expect, it } from "vitest";

it("swaps an object with an array", () => {
  const [store, setStore] = createStore<{ value: unknown }>({
    value: {},
  });

  setStore(reconcile({ value: [] }));
  flush();

  expect(Array.isArray(store.value)).toBe(true);
});

it("swaps an array with an object", () => {
  const [store, setStore] = createStore<{ value: unknown }>({
    value: [],
  });

  setStore(reconcile({ value: {} }));
  flush();

  expect(Array.isArray(store.value)).toBe(false);
});

it("reconciles an object with an array", () => {
  const [store, setStore] = createStore<{ value: unknown }>({
    value: { foo: "bar" },
  });

  const value = [0, 1, 2];
  setStore((draft) => {
    reconcile(value)(draft.value);
  });
  flush();

  expect(store.value).not.toEqual(value);
});

it("reconciles an array with an object", () => {
  const [store, setStore] = createStore<{ value: unknown }>({
    value: [0, 1, 2],
  });

  const value = { foo: "bar" };
  setStore((draft) => {
    reconcile(value)(draft.value);
  });
  flush();

  expect(store.value).not.toEqual(value);
});
