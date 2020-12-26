import { Elm } from './Main.elm'
import { startAudio } from './assets/audio'
import alarmAudioUrl from './assets/alarm.mp3'
import breakImageUrl from './assets/break.png'
import epilogueImageUrl from './assets/epilogue.png'
import pomodoroImageUrl from './assets/pomodoro.png'

class DocTitle extends HTMLElement {
  static observedAttributes = ['data-value']

  attributeChangedCallback(name, oldValue, newValue) {
    if (name === 'data-value') {
      document.title = newValue
    }
  }

  connectedCallback() {
    document.title = this.getAttribute('data-value')
  }
}

window.customElements.define('doc-title', DocTitle)

const app = Elm.Main.init({
  node: document.getElementById('app'),
  flags: {
    alarmAudioUrl,
    breakImageUrl,
    epilogueImageUrl,
    pomodoroImageUrl
  }
})


startAudio(app)
console.log(Notification.permission)
function askNotificationPermission() {
  function handlePermission(permission) {
    if(!('permission' in Notification)) {
      Notification.permission = permission;
    }
    const note = new Notification('Hello there', { tag: 'test' })
  }

  Notification.requestPermission(function(permission) {
    handlePermission(permission);
  })
}

askNotificationPermission()