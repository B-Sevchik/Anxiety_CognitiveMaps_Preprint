function practiceIllegalTransitionTask(){
  // for data logging
  sectionType = "pracTask";
  taskName = "practiceTransitionTask";

  //set up task
  hideCursor();
  hideInstructions();
  displayCanvases();
  selectFirstActiveNode();

  legalIllegalArray = preparePracticeArray();
  transitionType = legalIllegalArray[trialCount-1];
  transitionFunc = practiceTransition;
  taskFunc = runIllegalPractice;

  // start task after countdown
  countDown(runIllegalPractice, 3);
}

function runIllegalPractice(){
  if (trialCount <= nPracticeTrials) {
    practiceTrial();
  } else {
    practiceFeedback(Math.round( accCount / (trialCount - 1) * 100 ));
  }
}

function practiceTrial(){
  // check if key is being held down going into trial
  if (keyListener == 2 || keyListener == 3) {
    promptLetGo();
    return
  }

  // display network and image
  if (showNetworkWalk == true) {drawNetwork()}
  displayImage();

  // set up for response
  stimOnset = new Date().getTime() - runStart;
  respTime = NaN, partResp = NaN, respOnset = NaN, acc = NaN;
  if (trialCount == 1) {
    setTimeout(practiceTransition, stimInterval);
  } else {
    // need to proceed with key press
    keyListener = 1;
  }
}

function practiceTransition(){
  //determine accuracy
  if (transitionType == "l") {
    acc = ([90, 122].indexOf(partResp) != -1) ? 1 : 0;
  } else if (transitionType == "i") {
    acc = ([77, 109].indexOf(partResp) != -1) ? 1 : 0;
  }

  // determine if missed skip or false alarm
  missedSkip = (transitionType == "i" && !acc);
  falseAlarm = (transitionType == "l" && !acc);

  if (!feedbackShown && trialCount != 1) {

    showNetworkTransition(transitionType == "l")

  } else {

    adjustAccuracy();
    logIllegalTransitionData();
    rememberActiveNode();
    resetActiveNode();
    rememberTransitionType();
    chooseNewNode();

    activeNode.activate();
    trialHistory.push(activeNode.name);

    // iterate trial count
    trialCount++; blockTrialCount++;
    feedbackShown = false;

    // return to taskFlow func
    runIllegalPractice();
  }
}

function preparePracticeArray(){
  let nIllegalTrials = Math.ceil(nPracticeTrials*illegalProbability)
  let first_two = new Array(2).fill('l') //first two trials aren't illegal
  let arr = new Array(nIllegalTrials).fill('i').concat(new Array(nPracticeTrials - nIllegalTrials - 2).fill('l'));

  do {
    arr = shuffle(arr);
  } while (!arrShuffled(arr));

  return first_two.concat(arr);

  function arrShuffled(arr){
    // make sure there are no consecutive "i"s
    for (var i = 0; i < arr.length; i++) {
      if (i == 0) {continue}

      if (arr[i] == 'i' && arr[i - 1] == 'i') {
        return false;
      }
    }

    return true;
  }
}
