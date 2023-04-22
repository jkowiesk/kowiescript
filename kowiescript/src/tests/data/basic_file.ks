fn test() {
  print("test");
  ret 2;
}

const b = 3;

const files = ["test1", "test2"];

let a = test();
a = a + 1;

if a != 5  {
  print("'a' is not a five");
}

for file in files {
	// do sth with file
  if file == "end" {
  	end;
  }

  if file == "skip" {
  	next;
  }
}