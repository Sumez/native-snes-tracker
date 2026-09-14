# Native SNES Tracker (name pending)

## How to use
If you are experienced in other native trackers, \<insert name here\> should hopefully be familiar to you.
The control scheme is optimized to the SNES's 12-button D-pad. If you are using an emulator, it is recommended to use an actual video game controller over for more intuitive controls.

**D-pad**: Move the cursor around

**Select**: Hold to access the alternate features of other buttons
**Start**: Play the section you are currently editing. Starts song from current location, or loops the currently edited phrase indefinitely. Hold *Select* to play the current song from the start no matter where you are

**Y**: Inserts value in an empty space, automatically copying the last edited one. Hold to access "edit mode".
**X**: Deletes any inserted value, or inserts 

### Immediate roadmap:

- Known bugs
    - Navigating out of the instrument view after selecting a new sample often results in a crash
    - When playing back a row in song view, you'll some times experience a visual delay. This happens if every chain on the row is different, making it take a long time to load it into the APU
- ✅ Revised CPU controlled playback <sup>Done</sup>
    - ~~Start playback from any location in song~~ <sup>Done</sup>
    - ~~Pointer showing where in the song you are~~ <sup>Done</sup>
    - ~~Edit song, chains, etc. while playing, with immediate feedback~~ <sup>Done</sup>
    - ~~`Sound driver sending data back telling where in the current phrase it is`~~ <sup>Done</sup>
    - ~~`Only transfer one phrase per channel at a time to the APU, buffer one beat before it changes in SPC code`~~ <sup>Done</sup>
- Improved navigation
    - ~~Show adjacent views (chain preview from song view - chain and phrase are always visible at the same time)~~ <sup>Done</sup>
    - ~~Scroll up/down on song view~~ <sup>Done</sup>
        - Sprite based(?) discrete index numbers to the left of every 4th row (00..04..08..0C..10..14)
    - "Channel bar" in the top, facilitates navigating between channels using L/R buttons (probably)
    - ~~Navigate up down between sequential chains and phrases based on where in the song you are~~ <sup>Done</sup>
    - ~~Select multiple and copy/paste~~ <sup>Done</sup>
        - Bug: Pasting into a phrase while playback is active will not update the playing phrase, only the note where the cursor was
    - ~~Double tap selects next unused index~~
    - ~~Clone/deepclone inserted chains or phrases~~ <sup>Slim clone supported. Deep clone postponed until someone requests it</sup>
    - Make it visible ("greyed out"?) that a chain/phrase/instrument is "empty"/uncreated where selecting it
    - Hold Delete while moving cursor to delete multiple
    - Allow removing an already empty row in a song view channel to move everything below it up by one. Ideally a way to insert new row as well
- Playback adjustments according to UI
    - Select+Start should only play current song row when in the chain view
    - When a channel in the song playback hits an empty row, it should just loop back to the top used row before an unused one
- Echo filter
    - Set echo variables and enable per-channel using commands
    - Control *maximum* echo delay using a global variable - restricts usage of that specific command
    - Test echo directly in instrument editor
    - Custom FIR filters (sound modulation)
- Rudimentary GUI
    - More pleasant temporary graphics <sup>In progress</sup>
    - Save up sprite slots by having 32x32 sprites (a row playing in song view on all channels will now use 16 sprites plus potentially 2 from the cursor. Could easily be just 9)
    - Visible UI hints for easier navigation
    - Row and column labels <sup>In progress</sup>
        - Use sprites to display row indexes on song view?
    - Show more useful information on song view instead of having so many visible rows (maybe toggle-able?)
        - BPM, Song metadata, etc.
    - Highlight columns instead of rows on song view <sup>Unsure if that's even an improvement</sup>
    - `Convert tracker font to 2bpp and move to BG3 layer, use BG2 for UI graphics`
    - `Dynamic variable width font for UI elements`
- Basic tracker commands
    - Implement the ones already supported in the sound driver first - Pitch up/down, volume slide, arpeggio - loop and tempo change should be in song data instead? <sup>Partly implemented</sup>
        - ~~Pitch up/down~~
        - ~~Pan left/right~~
        - Volume (gain) up/down/set
        - Channel volume set
        - Tempo change
        - Loop data
        - Arpeggio
    - Delay
    - ~~"Key off" entries~~ <sup>Done</sup>
- ✅ Edit instruments in tracker <sup>Done</sup>
- Improve instrument editing
    - Select sample from visible list of every sample
    - ~~Alter custom pitch (transpose+finetune)~~ <sup>Done</sup>
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
    - ~~Improve SPC file ripping to avoid adding the same samples multiple times~~ <sup>Done</sup>
- Define loop points in samples
- Make various SNES specific features available, make controlling DSP registers more direct using commands
- Meta data
    - Edit song and author name
    - Assign name, and icon or color to instruments. Assign custom color to phrases and chains for easy recognition
- Save multiple songs to SRAM. Export/Import songs using Patcher application
- Simple view that vizualises APU memory used in both compressed and uncompressed form
    - Chain support in sound driver, handle looping from song data
    - `Generate compressed song data from tracker. Use note progression macros and silent blocks`
- Advanced tracker commands
    - Sample offset
    - Pitch modulation

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
- A simultaneous view of all >=8 chains playing at the same time according to the song view