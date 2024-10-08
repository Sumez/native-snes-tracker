# Native SNES Tracker (name pending)

### Immediate roadmap:

- ✅ Revised CPU controlled playback <sup>Done</sup>
    - ~~Start playback from any location in song~~ <sup>Done</sup>
    - ~~Pointer showing where in the song you are~~ <sup>Done</sup>
    - ~~Edit song, chains, etc. while playing, with immediate feedback~~ <sup>Done</sup>
    - ~~`Sound driver sending data back telling where in the current phrase it is`~~ <sup>Done</sup>
    - ~~`Only transfer one phrase per channel at a time to the APU, buffer one beat before it changes in SPC code`~~ <sup>Done</sup>
- Improved navigation
    - ~~Show adjacent views (chain preview from song view - chain and phrase are always visible at the same time)~~ <sup>Done</sup>
    - "Channel bar" in the top, facilitates navigating between channels using L/R buttons (probably)
    - ~~Navigate up down between sequential chains and phrases based on where in the song you are~~ <sup>Done</sup>
    - Fresh insert on song or chain view will insert the next unused index
    - Clone/deepclone inserted chains or phrases
    - Hold Delete while moving cursor to delete multiple
    - Make it visible ("greyed out"?) that a chain/phrase/instrument is "empty"/uncreated where selecting it
- Rudimentary GUI
    - More pleasant temporary graphics <sup>In progress</sup>
    - Visible UI hints for easier navigation
    - Row and column labels <sup>In progress</sup>
    - Highlight columns instead of rows on song view
    - `Convert tracker font to 2bpp and move to BG3 layer, use BG2 for UI graphics`
    - `Dynamic variable width font for UI elements`
- Basic tracker commands
    - Implement the ones already supported in the sound driver first - Pitch up/down, volume slide, arpeggio - loop and tempo change should be in song data instead? <sup>Partly implemented</sup>
    - ~~"Key off" entries~~ <sup>Done</sup>
- ✅ Edit instruments in tracker <sup>Done</sup>
- Improve instrument editing
    - Select sample from visible list of every sample
    - Alter custom pitch (transpose+finetune)
    - Alter instrument while song is playing for immediate feedback
    - Define volume envelope
- Improved sound driver
    - Better use of keyoff/keyon control instead of manual volume
    - ADSR support on instruments
    - Transpose chains
- ✅ Companion software for adding custom samples <sup>Done</sup>
    - ~~Ability to add as many samples as ExHiRom can fit~~ <sup>Done</sup>
    - ~~Manually pick which samples to include in a song in the song settings inside the tracker~~ <sup>Done</sup>
    - ~~`Rewrite to use HiRom or ExHiRom`~~ <sup>Done</sup>
    - Improve SPC file ripping to avoid adding the same samples multiple times
- Define loop points in samples
- Make various SNES specific features available, make controlling DSP registers more direct using commands
- Meta data
    - Edit song and author name
    - Assign name, and icon or color to instruments. Assign custom color to phrases and chains for easy recognition
- Save multiple songs to SRAM. Export/Import songs using Patcher application
- Simple view that vizualises APU memory used in both compressed and uncompressed form
    - Chain support in sound driver, handle looping from song data
    - `Generate compressed song data from tracker. Use note progression macros and silent blocks`

### Long term goals:

- Pretty graphics
- Tomas makes a mascot sprite
- Hardware echo filter
- Pitch modulation
- Built-in sample synthesis
- Preloaded royalty-free default samples
- Hardware noise channel
- Live mode
- Pre-emphasize samples to counterbalance the SNES DSP's gaussian filter
- VJ mode?
- 2-Player controls
