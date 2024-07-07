function illegalTransitionTask(){

  // for data logging
  sectionType = "mainTask";
  taskName = "illegalTransitionTask";

  // set up task
  hideCursor();
  hideInstructions()
  displayCanvases();
  selectFirstActiveNode();

  legalIllegalArray = prepareLegalIllegalArray();
  transitionType = legalIllegalArray[trialCount-1];
  transitionFunc = networkTransition;
  taskFunc = runIllegalTransition;

  // start task after countdown
  countDown(runIllegalTransition, 3);
}

function runIllegalTransition(){
  if (trialCount > nNetworkTrials) {
    navigateInstructionPath();
    return
  }

  // go to break screen every n trials, but only if trial count > 1, its not the end of the task (trialCount == nNetworkTrials), and we didn't JUST do have a break screen (breakOn)
  if (trialCount > 1 && trialCount != nNetworkTrials && (trialCount - 1)%breakEveryNTrials == 0 && !breakOn) {
    breakOn = true;
    blockBreak(nNetworkTrials, breakEveryNTrials);
    return
  }

  // else proceed to trial
  breakOn = false;
  networkTrial();
}

function networkTrial(){
  if (keyListener == 2 || keyListener == 3) {
    //if key is being held down, prompt let go
    promptLetGo();
    return
  }

  // display network and image
  if (showNetworkWalk) drawNetwork();
  requestAnimationFrame(displayImage);

  // set up for response
  stimOnset = new Date().getTime() - runStart;
  keyListener = 1, respTime = NaN, partResp = NaN, respOnset = NaN, acc = NaN;

  // go to next trial after delay
  setTimeout(networkTransition, stimInterval);
}

function networkTransition(){
  if (transitionType == "i") {
    acc = (partResp) ? 1 : 0;
  } else {
    acc = (partResp) ? 0 : 1;
  }

  // determine if missed skip or false alarm
  missedSkip = (transitionType == "i" && !acc);
  falseAlarm = (transitionType == "l" && !acc);

  if (!acc && !feedbackShown && showFeedback) {

    showNetworkTransition(transitionType == "l")

  } else {

    // log this trial and proceed to next by choosing a new node
    logIllegalTransitionData();
    rememberActiveNode();
    resetActiveNode();
    chooseNewNode();

    trialCount++; blockTrialCount++;
    activeNode.activate();
    trialHistory.push(activeNode.name);
    feedbackShown = false;

    // return to taskFlow func
    runIllegalTransition();
  }
}

function prepareLegalIllegalArray(){
  let batchSize = 20;
  let nBlocks = Math.ceil(nNetworkTrials/breakEveryNTrials);
  let batch, mainArr = [];
  // for each block
  for (let i = 0; i < nBlocks; i++) {
    let arr = [];
    let nBatches = Math.ceil(breakEveryNTrials/batchSize);

    // first batch (make sure first 2 trials aren't illegal)
    do {
      batch = getBatch();
    } while (batch[0] == 'i' || batch[1] == 'i');
    arr = arr.concat(batch);

    // rest of batches
    for (let j = 0; j < nBatches - 1; j++) {
      do {
        batch = getBatch();
      } while (batch[0] == 'i' && arr[arr.length - 1] == 'i');
      arr = arr.concat(batch);
    }

    mainArr = mainArr.concat(arr.slice(0, breakEveryNTrials));
  }

  // S.O. code for checking instances of 'i' and 'l'
  //   console.log(mainArr.reduce(function (arr, i) {
  //   return arr[i] ? ++arr[i] : arr[i] = 1, arr
  // }, {}));
  return mainArr;

  function getBatch(){
    let nIllegalTrials = Math.ceil(batchSize*illegalProbability)
    let arr = new Array(nIllegalTrials).fill('i').concat(new Array(batchSize - nIllegalTrials).fill('l'));
    do {
      arr = shuffle(arr);
    } while (!arrShuffled(arr));
    return arr;

    function arrShuffled(arr){
      // make sure there are no consecutive "i"s
      for (let i = 0; i < arr.length; i++) {
        if (i == 0) {continue}

        if (arr[i] == 'i' && arr[i - 1] == 'i') {
          return false;
        }
      }

      return true;
    }
  }
}
