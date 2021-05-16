import xs from 'xstream';
import { run } from '@cycle/run';
import { makeDOMDriver } from '@cycle/dom';
import { makeHTTPDriver } from '@cycle/http';
import view from './view.js';
import _ from 'lodash';

function main(sources) {
  //get responses
  const postQueueResponse$ = sources.HTTP
          .select('post-queue')
          .map((response$) =>
            response$.replaceError(() => xs.of(errorObject))
          )
          .flatten()
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

  // an event every 5 seconds or when a postQueueResponse is obtained
  const update$ = xs.merge(xs.periodic(5000), postQueueResponse$)

  //get jobs once
  const jobRequest$ = xs.of({
            category: 'get-job',
            url: BASE_URL + '',
            method: 'GET'
          });

  // get queue after every event in update (so every 5 seconds and whenever a postQueueResponse is obtained)
  const queueRequest$ = update$.mapTo({
            category: 'get-queue',
            url: BASE_URL + '/queue',
            method: 'GET'
          });

  // get state after every event in update (so every 5 seconds and whenever a postQueueResponse is obtained)
  const stateRequest$ = update$.mapTo({
            category: 'get-state',
            url: BASE_URL + '/state',
            method: 'GET'
          });

  // send POST request whenever enqueue-button is clicked
  const postQueue$ = sources.DOM.select('.enqueue-button').events('click')
          .map(x => {
            console.log(x.target);
            return x;
          })
          .map(x => x.target.dataset.name)
          .map(x => ({
            category: 'post-queue',
            url: BASE_URL + '/queue',
            send: {
              job: x
            },
            method: 'POST'
          }));

  // get responses from GET requests
  const jobResponse$ = sources.HTTP
          .select('get-job')
          .flatten()
          .map(x => ({jobTypes: x.body.jobTypes, information: x.body.information}));
  const queueResponse$ = sources.HTTP
          .select('get-queue')
          .flatten()
          .map(x => ({queue: x.body.queue}));
  const stateResponse$ = sources.HTTP
          .select('get-state')
          .flatten()
          .map(x => ({running: x.body.running}));

  // combine all responses, model$ has the latest values from the 3 response streams
  const model$ = xs.combine(
    jobResponse$,
    queueResponse$,
    stateResponse$,
  )
  .map(arr => _.assign(arr[0], arr[1], arr[2]));

  //MDL temporary
  setTimeout(() => {
    componentHandler.upgradeDom();
  }, 100);

  // assign latest model to the view
  // start the HTTP driver for sending requests
  return {
    DOM: model$.map(view),
    HTTP: xs.merge(jobRequest$, queueRequest$, stateRequest$, postQueue$)
  };
}

const drivers = {
  DOM: makeDOMDriver('#app'),
  HTTP: makeHTTPDriver()
};

run(main, drivers);
