# musix
Musix is a tool I've been working on sporadically for some time in a few different languages (C#, F#, Haskell) - this iteration is in Haskell using OpenGL and Yampa (a Functional Reactive Programming library). The idea is to use FRP to process a MIDI stream as you're playing an instrument and give you feedback about your playing. Currently you program in a scale you want to play in, and it gives you visual feedback if you play a note that isn't in the scale.

You enter scale selection mode by playing the same chord 3 times quickly, and then simultaneously play enough notes to uniquely identify the scale, with the lowest note being the root of the scale.

![Musix Screenshot](docs/img/musix.png?raw=true "Screenshot of Musix in action")
