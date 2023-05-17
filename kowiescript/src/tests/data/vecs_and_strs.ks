fn starts_with_vowel(word) {
  const first_letter = word[0];
  for vowel in ["a", "e", "i", "o", "u"] {
  		if first_letter == vowel {
  			ret true;
  		}
  }
  ret false;
}

let words = ["apple", "cat", "dog", "egg", "fish", "sheep"];
let vowel_words = [];

for word in words {
  if starts_with_vowel(word) {
    push("vowel_words", word);
  }
}