load('AllDataFrame_alsoBad.RData')
write.table(allDat,paste0(format(Sys.time(), '%Y-%m-%d'),'_allDat.csv'))


#'| Predictor  |  Explanation |
#'|----------  |  ------------ |
#'| trial  |  Trial Number (1 to 720, in some subject 1 to 360) |
#'| stimLLoc |  Is the left stimulus in the temporal field (1) or nasal (0)
#'| stimRLoc |  s.a.
#'| stimLCP | Is the left stimulus continuous (0) or perpendicular (1)
#'| stimRCP | s.a.
#'| stimPVisible |  Is the orthogonal stimulus visible (2), visible but filled in in the blind spot (1), or both stimuli are continuous (0)
#'| stimOrient |  the outer stimulus is horizonal (1) or vertical (0)
#'| ISI |  Inter stimulus interval between ?
#'| bsXL |  x-position of BS Left in px (careful, in Katjas experiment we put subjects closer to the monitor!
#'| bsXR |  s. a.
#'| bsYL |  s. a.
#'| bsYR |  s. a.
#'| bsSL |  size of left BS
#'| bsSR |  size of right BS
#'| block |  block of experiment
#'| success |  did the trial finish correctly?
#'| answer |  answer, 0 -> left, 1 -> right
#'| oneBack |  the answer in the previous trial (imputed numbers on first trial / erroneous previous trial by choosing the previous last available answer, thus no NAs)
#'| correct |  subject answered correctly (CAREFUL: if two continuous or the orthogonal is filled-in, the answer is counted as correct)
#'| rt |  Reactiontime in s
#'trialTime |  How long the whole trial took
#'controlTrial |  stimulus locaiton, 0 -> outward, 1-> BS, 2-> Inward, 3->Above
#'subject |  Subject-Identification, depending on experiment different schema. see Subject Overview_v3.xlsx in the SVN for more information
#'age |  age of participant
#'sex |  sex
#'handedness |  left => 0, right => 1
#'dominantEye |  left 0 right 1
#'experiment |  Control, EEG (EEG + Replication pooled), Horizontal
#'remove |  whether to remove the trial due to any reason
#'removeReason |  the reason why the trial should be removed
