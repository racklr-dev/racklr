// A tiny program that exercises functions, literals, and an effectful call.
function greet(name) {
  return "Hello, " + name + "!";
}

function report(name) {
  console.log(greet(name));
  return name;
}

var who = "world";
report(who);
