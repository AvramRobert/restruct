## restruct

CLI for changing the directory structure of sound packs / sound kits by means of provided patterns.

### Usage

Given some structured directory of sound files, generally taxonomised by type:
```text
/soundkit
    |
    |---/bass
    |     |
    |     |---/bass-1 120bpm Cmin.wav
    |     |---/bass-2 120bpm Cmin.wav
    |      
    |---/synth
    |     |
    |     |---/synth-1 lead 87bpm Dmaj.wav
    |     |---/synth-2 grimey 80bpm Dmin.wav       
    |
    ...
(... or any structure for that matter)
```
Provide patterns of how the current structure looks like, read it in and transform it into a more convenient structure:
```bash
> restruct --dir=./ \
           --dir-structure='<maker>/<type>' \
           --file-pattern='<type> <feel> <tempo> <key>.<ext>' \
           --dir-restructure='<type>/<tempo>/<key>' \
           --rename-pattern='<maker>-<name>-<type>.<ext>'
```
Output:
```text
/bass
   |
   |---/120bpm
   |     |
   |     |---/Cmin
   |     |     |
   |     |     |---/soundkit-bass-1.wav
   |     |     |---/soundkit-bass-2.wav
   |        
/synth
   |
   |---/87bpm
   |     |
   |     |---/Dmaj
   |     |    |
   |     |    |---/soundkit-synth-1-lead.wav
   |        
   |---/80bpm
   |     |
   |     |---/Dmin
   |     |     |
   |     |     |---/soundkit-synth-2-grimey.wav
```