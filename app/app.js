import Rx from 'rx';
import Cycle from '@cycle/core';
import CycleDOM from '@cycle/dom';
import {makeHTTPDriver} from '@cycle/http';
import view from './view.jsx';
import _ from 'lodash';

var BASE_URL = 'http://localhost:3080/api/v0/job';

function main(sources) {
  //get responses
  const postQueueResponse$ = sources.HTTP
          .filter(res$ => res$.request.id == 'post-queue')
          .flatMap((x$) =>
                   x$
                   .catch(e => Rx.Observable.just(e))
                  )
          .map(x => {
            //!! unsafePerformIO
            console.log(x);
            var snackbarContainer = document.querySelector('#demo-toast-example');
            var data = x.statusCode == 200 ?
                  {message: 'SUCCESS'} :
                {message: x};
            snackbarContainer.MaterialSnackbar.showSnackbar(data);
            return x;
          });
  //update stream, publish
  const update$ = postQueueResponse$
          .merge(Rx.Observable
                 .interval(5000)
                )
          .publish();
  
  //make requests
  const jobRequest$ = Rx.Observable.just(0)
          .map({
            id: 'get-job',
            url: BASE_URL + '',
            method: 'GET'
          });
  const queueRequest$ = Rx.Observable.just(0)
          .merge(update$)
          .map({
            id: 'get-queue',
            url: BASE_URL + '/queue',
            method: 'GET'
          });
  const stateRequest$ = Rx.Observable.just(0)
          .merge(update$)
          .map({
            id: 'get-state',
            url: BASE_URL + '/state',
            method: 'GET'
          });
  // POST request
  const postQueue$ = sources.DOM.select('.enqueue-button').events('click')
          .map(x => {
            console.log(x.target);
            return x;
          })
    .map(x => x.target.dataset.name)
    .map(x => ({
      id: 'post-queue',
      url: BASE_URL + '/queue',
      send: {
        job: x
      },
      method: 'POST'
    }));

  //merge responses
  const jobResponse$ = sources.HTTP
          .filter(res$ => res$.request.id == 'get-job')
          .mergeAll()
          .map(x => ({jobTypes: x.body.jobTypes}));
  const queueResponse$ = sources.HTTP
          .filter(res$ => res$.request.id == 'get-queue')
          .mergeAll()
          .map(x => ({queue: x.body.queue}));
  const stateResponse$ = sources.HTTP
          .filter(res$ => res$.request.id == 'get-state')
          .mergeAll()
          .map(x => ({running: x.body.running}));
  
  const model$ = Rx.Observable.combineLatest(
    jobResponse$,
    queueResponse$,
    stateResponse$,
    _.assign
  );

  //connect
  update$.connect();
  
  //MDL temporary
  setTimeout(() => {
    componentHandler.upgradeDom();
  }, 100);
  
  return {
    DOM: model$.map(view),
    HTTP: Rx.Observable.merge(jobRequest$, queueRequest$, stateRequest$, postQueue$)
  };
}

const drivers = {
  DOM: CycleDOM.makeDOMDriver('#app'),
  HTTP: makeHTTPDriver()
};

Cycle.run(main, drivers);

