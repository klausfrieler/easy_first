# Easy First
 Analyzing sequencing bias in jazz solo improvations with respect to intrinsinc and extrinsic easiness. See https://psyarxiv.com/qdh32/
 for a preprint, check https://jazzomat.hfm-weimar.de/easy_first/ for the Shiny app contained in here.
 
 To start analyzing, run ``setup_workspace()`` from ``analysis.R?? first. The data files are very large and thus stored on OSF and not in the git-repo (except wjd_metadata.csv) and will be downloaded if not present on the local installation (this might take some time).
 
 
 
 More documentation to follow.

## Dependency to other R packages
 ``` r
 install.packages("tidyverse")
 install.packages("ggrepel")
 install.packages("osfr")
 install.packages("shiny")
 install.packages("shinythemes")
 ```

## Data objects
Central data object is ``wjd_all``, where all data is stored in one large (~2 mio rows) data object. A row pertains to a single N-gram instance, with added metadata on several levels N-gram (set of all N-grams with the same value), containing phrase and containing solo.
 
Here is a short description of all variables in ``wjd_all``:
 
 
|Column            |Description |
|:-----------------|:--------|
|``id``                | Unique ID of the containing solo         |
|``start``             | Start position of N-gram in containing solo         |
|``N``                 | Length of N-gram instance (1-10)         |
|``onset``             | Onset (s) of N-gram in containing solo         |
|``dur``               | Dur (s) of N-gram instance in containing solo        |
|``metricalposition``  | Metrical position of N-gram instance in WJD short notation         |
|``value``             | Value of N-gram instance as comma separated list of semitone intervals         |
|``freq``              | Frequency of N-gram         |
|``prob100``           | Rel. frequency of N-gram (as percentage)         |
|``number_notes``      | Number of notes of containing solo         |
|``melid``             | Numerical unique ID of containing solo          |
|``instance_id``       | Unique ID of this N-gram instance (format ``<melid>-<N>-<start>``         |
|``DF``                | Number of solos containing the N-gram         |
|``TF``                | Term frequency of this N-gram         |
|``max_TF``            | ?         |
|``nTF``               | ?         |
|``nTF2``              | ?         |
|``performer``         | Performer of containing solo         |
|``recordingyear``     | Recording year of containing solo       |
|``style``             | Style of containing solo         |
|``performer_style``   | Combination of performer and style of containing solo         |
|``main_type``         | Main type of N-gram: ``cool`` ( = subtype``other`` ), or ``uncool`` ( = subtype ``scale``, ``trill``, ``repetition``)         |
|``sub_type``          | Pattern (sub) type: ``other``, ``scale``, ``trill``, ``repetition``        |
|``start_id``          | ID of solo positions in format ``<melid>-<start>``         |
|``phrase_pos``        | Start position of N-gram instance in containing phrase (``0-length(phrase)``)         |
|``rev_phrase_pos``    | Start positon of N-gram measured from end of containing phrase (``length(phrase)-phrase_pos``)        |
|``rel_phrase_pos``    | Relative start position in containing phrase (Values in ``0-1``)          |
|``phrase_len``        | Length of containing phrase         |
|``in_phrase``         | Flag whether N-gram instance is contained fully in a single phrase         |
|``pos_weight``        | ?       |
|``mean_int_size``     | Mean interval size of N-gram         |
|``int_range``         | Interval range ``(max(int)- min(int))`` of the N-gram         |
|``dir_change``        | Number of direction changes       |
|``int_variety``       | Interval variety (relative number of different interval)         |
|``pitch_variety``     | Relative number of different pitches in N-gram       |
|``mean_run_length``   | Mean run length of intervals in N-gram         |
|``surprise``          | Inverse logarithm of N-gram probability (rel. freq) ``-log(prob100*100)``        |
|``g_phrase_id``       | Unique ID for containing phrase          |
|``phrase_id_raw``     | Original running number of containing phrase in containing solo          |
|``prob_p``            | Probability of the N-gram for the performer of containing solo      |
|``surprise_p``        | ``- log(prob_p)`` |           |
|``prob_s``            | Relative frequency of the N-gram in containing solo         |
|``surprise_s``        | ``- log(prob_s)``        |
|``combined_easiness`` | Combined easiness (intrinsic easiness, see preprint)         |
|``z_surprise``        | N-wise z-transform of ``suprise``         |
|``z_int_variety``     | N-wise z-transform of ``int_variety``        |
|``z_pitch_variety``   | N-wise z-transform of ``pitch_variety``         |
|``z_dir_change``      | N-wise z-transform of ``dir_change``         |
|``z_mean_run_length`` | N-wise z-transform of ``mean_run_length``         |
|``avgtempo``          | Average Tempo of containing solo         |
|``tempoclass``        | Tempo class of containing solo ``SLOW``, ``MEDIUM SLOW``, ``MEDIUM``, ``MEDIUM UP``, ``UP``         |