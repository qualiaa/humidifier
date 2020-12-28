# Humidifier

Humidifier is a tool to normalise the key of a MIDI file and plot the result as
a piano roll.

It was built for visually comparing vocal lines between different songs - quite
a niche!

Should work out of the box with Haskell Stack or Cabal.

## Builds

Windows builds are available in releases. On Linux it should be fairly easy to
build with Haskell Stack (`stack build`).

## Usage

See

```
humidify --help
```

for usage options.

Setting the beat unit and beats per bar will probably break things at the moment.


## Example Output

```
humidify example/08\ Because\ -\ Harmony\ Vocal\ 1.mid C# -w 4000
```

![](https://github.com/qualiaa/humidifier/blob/master/example/08%20Because%20-%20Harmony%20Vocal%201.svg?raw=true)


```
humidify example/08\ Because\ -\ Harmony\ Vocal\ 1.mid C# --start-bar 10 --end-bar 18
```

![](https://github.com/qualiaa/humidifier/blob/master/example/08%20Because%20-%20Harmony%20Vocal%201%20-%2011-19.svg?raw=true)
