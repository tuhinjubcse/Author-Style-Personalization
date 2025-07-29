import os
import json
import random
import threading
import logging
from datetime import datetime
from collections import defaultdict
from flask import (
    Flask, render_template, make_response, jsonify,  request,
    redirect, url_for, session, flash
)
from filelock import FileLock

app = Flask(__name__)
app.secret_key = 'your-secret-key'  # Change this in production

# --- 1) Style-only user credentials ---
users = {
    "Celeste": "Celestexv129",
    "Sophia": "Sophiaub456",
    "Adesuwa_Agbonile": "Adesuwawz567",
    "Sean_Cavanaugh": "Seanyh872",
    "Rachael": "Rachaellk907",
    "James_Braun": "Jamescx410",
    "James_Cato": "Jamesgh567",
    "Harry": "Harrybh639",
    "Hank": "Hankkq837",
    "Renne_Flory": "Renneut437",
    "Connor": "Connorsd615",
    "Caroline_Waring": "Carolinefp417",
    "Caroline_Porter": "Carolineyv953",
    "Benjamin": "Benjamingv678",
    "Ashni": "Ashnisk741",
    "Devanshi": "Devanshilo277",
    "Cara": "Carart874",
    "Sara": "Sarayt811",
    "Brianna_Uzoh": "Brianna556",
    "Yash": "Yashuj765",
    "Devin": "Devinld484",
    "Tian": "Tianxw655",
    "Stefan": "Stefanxc771",
    "Yen": "Yenop147",
    "Lior": "Liored548",
    "Natalie": "Nataliexn751",
    "Jon": "Joncv680",
    "Hayden": "Haydengh661",
    "Lauren": "Laurenop793",
    "Brandyn": "Brandyntt761",
    "Mar": "Markl908",
    "dhillonp": "dhillonp123"
}

style_users = [
    'Connor', 'Harry', 'Caroline_Waring','Celeste', 'Sara', 'Benjamin','Sean_Cavanaugh', 'Tian',
    'Caroline_Porter', 'Yen', 'Yash',
    'Hayden', 'Lauren', 'Brandyn', 'Anne'
]

# --- 2) Combined-user pool and locking ---
AVAILABLE_USERS_PATH = 'available_users.json'
LOCK_PATH = AVAILABLE_USERS_PATH + '.lock'

# --- 2b) Prolific PID → assigned username mapping ---
MAPPING_PATH = 'participant_map.json'
MAPPING_LOCK_PATH = MAPPING_PATH + '.lock'

# Thread-lock for writing responses per user
user_locks = defaultdict(threading.Lock)

# --- 3) Dedicated allocations logger ---
alloc_logger = logging.getLogger('allocations')
alloc_logger.setLevel(logging.INFO)
alloc_fh = logging.FileHandler('allocations.log')
alloc_fh.setFormatter(logging.Formatter('%(asctime)s  %(message)s', '%Y-%m-%dT%H:%M:%S'))
alloc_logger.addHandler(alloc_fh)
alloc_logger.propagate = False


@app.route('/')
def home():
    resp = {
        'message': 'Welcome',
        'code': 'SUCCESS'
    }
    return make_response(jsonify(resp), 200)

@app.route('/start', methods=['GET'])
def start():
    # grab Prolific’s URL‐substitution tags
    pid      = request.args.get('PROLIFIC_PID')
    study_id = request.args.get('STUDY_ID')
    sess_id  = request.args.get('SESSION_ID')
    
    print(pid)
    if not pid:
        return "Missing participant_id", 400

    # ensure mapping file exists
    if not os.path.exists(MAPPING_PATH):
        with open(MAPPING_PATH, 'w') as mf:
            json.dump({}, mf)

    # load mapping
    with FileLock(MAPPING_LOCK_PATH, timeout=5):
        with open(MAPPING_PATH, 'r') as mf:
            part_map = json.load(mf)

    if pid in part_map:
        # returning user: restore their slot & index
        session.clear()
        session['participant_id'] = pid
        session['study_id']       = part_map[pid].get('study_id')
        session['session_id']     = part_map[pid].get('session_id')
        session['username']       = part_map[pid]['username']
        # recompute how many they've already done
        responses_file = os.path.join('annotator_files', f"{session['username']}_responses.json")
        idx = 0
        if os.path.exists(responses_file):
            with open(responses_file) as rf:
                saved = json.load(rf)
            idx = len(saved)
        session['index'] = idx
        return redirect(url_for('review_both'))

    # first‐time user: store PID & wait for consent
    session.clear()
    session['participant_id'] = pid
    session['study_id']       = study_id
    session['session_id']     = sess_id
    return render_template('landing.html')


@app.route('/consent', methods=['POST'])
def consent():
    choice = request.form.get('consent')
    if choice != 'yes':
        flash("You must consent to participate.")
        return redirect(url_for('home'))

    # Pop one ID atomically from the pool
    lock = FileLock(LOCK_PATH, timeout=5)
    with lock:
        with open(AVAILABLE_USERS_PATH, 'r+') as pf:
            pool = json.load(pf)
            if not pool:
                return "Sorry, no more slots available.", 503
            assigned = pool.pop(random.randrange(len(pool)))
            pf.seek(0)
            json.dump(pool, pf, indent=2)
            pf.truncate()

    # record the mapping for future visits
    with FileLock(MAPPING_LOCK_PATH, timeout=5):
        with open(MAPPING_PATH, 'r+') as mf:
            part_map = json.load(mf)
            part_map[session['participant_id']] = {
                "username":   assigned,
                "study_id":   session.get('study_id'),
                "session_id": session.get('session_id')
            }
            mf.seek(0)
            json.dump(part_map, mf, indent=2)
            mf.truncate()

    # log allocation with Prolific ID
    alloc_logger.info(json.dumps({
        "participant_id": session.get('participant_id'),
        "assigned_id":    assigned
    }))

    # store assignment in session
    session['username'] = assigned
    session['index'] = 0
    session.pop('break_passed', None)

    return redirect(url_for('review_both'))


@app.route('/login', methods=['GET', 'POST'])
def login():
    if request.method == 'POST':
        username = request.form.get('username', '').strip()
        password = request.form.get('password', '')

        if not username or not password:
            flash("Both username and password are required!")
            return redirect(url_for('login'))

        if username not in users:
            flash("Username not found.")
            return redirect(url_for('login'))
        if users[username] != password:
            flash("Incorrect password.")
            return redirect(url_for('login'))

        session.clear()
        session['username'] = username
        session.pop('break_passed', None)

        # Resume progress
        responses_file = f"{username}_responses.json"
        if os.path.exists(responses_file):
            with open(responses_file, 'r') as rf:
                saved = json.load(rf)
            session['index'] = len(saved)
        else:
            session['index'] = 0

        if username in style_users:
            return redirect(url_for('review_style'))
        else:
            return redirect(url_for('review'))

    return render_template('login.html')


@app.route('/debug_session')
def debug_session():
    return f"user={session.get('username')!r} | index={session.get('index')!r}"


@app.route('/review', methods=['GET', 'POST'])
def review():
    if 'username' not in session:
        return redirect(url_for('login'))
    username = session['username']
    json_filename = f"{username}.json"
    if not os.path.exists(json_filename):
        return f"File {json_filename} not found.", 404

    with open(json_filename) as f:
        entries = json.load(f)

    idx = session.get('index', 0)
    total = len(entries)

    if request.method == 'POST':
        pref = request.form.get('preference')
        reason = request.form.get('reason', '').strip()
        if not pref or not reason:
            flash("Please select a preference and provide a reason.")
            return redirect(url_for('review'))

        current = entries[idx]
        resp = {
            "id": current["id"],
            "user": username,
            "Paragraph1": current["Paragraph1"],
            "Paragraph2": current["Paragraph2"],
            "key": current["key"],
            "Preference": pref,
            "Reason": reason
        }

        responses_file = f"{username}_responses.json"
        with user_locks[username]:
            saved = []
            if os.path.exists(responses_file):
                with open(responses_file) as rf:
                    saved = json.load(rf)
            saved.append(resp)
            with open(responses_file, 'w', encoding='utf-8') as wf:
                json.dump(saved, wf, ensure_ascii=False, indent=4)

        session['index'] = idx + 1
        if session['index'] >= total:
            return redirect(url_for('complete'))
        return redirect(url_for('review'))

    if idx >= total:
        return redirect(url_for('complete'))
    return render_template('review.html', entry=entries[idx])


@app.route('/review_style', methods=['GET', 'POST'])
def review_style():
    if 'username' not in session:
        return redirect(url_for('login'))
    username = session['username']
    json_filename = f"{username}.json"
    if not os.path.exists(json_filename):
        return f"File {json_filename} not found.", 404

    with open(json_filename) as f:
        entries = json.load(f)

    idx = session.get('index', 0)
    total = len(entries)

    if request.method == 'POST':
        pref = request.form.get('preference')
        reason = request.form.get('reason', '').strip()
        if not pref or not reason:
            flash("Please select a preference and provide a reason.")
            return redirect(url_for('review_style'))

        current = entries[idx]
        resp = {
            "id": current["id"],
            "user": username,
            "Original": current["Original"],
            "Paragraph1": current["Paragraph1"],
            "Paragraph2": current["Paragraph2"],
            "key": current["key"],
            "Preference": pref,
            "Reason": reason
        }

        responses_file = f"{username}_responses.json"
        with user_locks[username]:
            saved = []
            if os.path.exists(responses_file):
                with open(responses_file) as rf:
                    saved = json.load(rf)
            saved.append(resp)
            with open(responses_file, 'w', encoding='utf-8') as wf:
                json.dump(saved, wf, ensure_ascii=False, indent=4)

        session['index'] = idx + 1
        if session['index'] >= total:
            return redirect(url_for('complete'))
        return redirect(url_for('review_style'))

    if idx >= total:
        return redirect(url_for('complete'))
    return render_template('review_style.html', entry=entries[idx])


@app.route('/review_both', methods=['GET', 'POST'])
def review_both():
    if 'username' not in session:
        return redirect(url_for('login'))
    username = session['username']

    json_path = os.path.join('annotator_files', f"{username}.json")
    if not os.path.exists(json_path):
        return f"File {json_path} not found.", 404

    with open(json_path) as f:
        entries = json.load(f)

    idx = session.get('index', 0)
    total = len(entries)

    # 1) Handle break page form submit
    #if request.method == 'POST' and request.form.get('break_continue'):
    #    session['break_passed'] = True
    #    return redirect(url_for('review_both'))

    # 2) After 10 quality evals, show break page (but only once)
    #if idx == 10 and not session.get('break_passed'):
    #    return render_template('break.html')

    # 3) Handle normal review posts
    if request.method == 'POST':
        pref = request.form.get('preference')
        reason = request.form.get('reason', '').strip()
        if not pref or not reason:
            flash("Please select a preference and provide a reason.")
            return redirect(url_for('review_both'))

        current = entries[idx]
        resp = {
            "id":             current["id"],
            "participant_id": session.get('participant_id'),
            "timestamp" : datetime.now().strftime("%m/%d/%Y, %H:%M:%S"),
            "user":           username,
            "Paragraph1":     current["Paragraph1"],
            "type" : current["type"],
            "Paragraph2":     current["Paragraph2"],
            "key":            current["key"],
            "Preference":     pref,
            "Reason":         reason
        }
        if idx >= 1 and username!='6UI3edt':
            resp["Original"] = current["Original"]

        if username=='6UI3edt' and idx>=2:
            resp["Original"] = current["Original"]

        responses_path = os.path.join('annotator_files', f"{username}_responses.json")
        with user_locks[username]:
            saved = []
            if os.path.exists(responses_path):
                with open(responses_path) as rf:
                    saved = json.load(rf)
            saved.append(resp)
            with open(responses_path, 'w', encoding='utf-8') as wf:
                json.dump(saved, wf, ensure_ascii=False, indent=4)

        session['index'] = idx + 1
        if session['index'] >= total:
            return redirect(url_for('complete'))
        return redirect(url_for('review_both'))

    # 4) GET: render quality vs style templates
    if idx < 1 and username!='6UI3edt':
        return render_template('review.html', entry=entries[idx])
    else:
        if idx < 2 and username=='6UI3edt':
            return render_template('review.html', entry=entries[idx])
        else:
            return render_template('review_style.html', entry=entries[idx])


@app.route('/complete')
def complete():
    return render_template('complete.html')


@app.route('/healthz')
def healthz():
    return 'OK', 200

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=80, debug=True)
