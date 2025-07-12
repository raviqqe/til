Given("{string} is {string}") do |x, y|
  expect(x).to eq(y)
end

Given("{string} is a newline") do |x|
  expect(x).to eq("\n")
end
