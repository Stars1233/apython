_warnings_list = []
_filters_action = 'default'

def warn(message, category=None, stacklevel=1):
    if _filters_action == 'ignore':
        return
    if category is not None:
        _warnings_list.append(category)
    else:
        _warnings_list.append(message)

def simplefilter(action):
    global _filters_action
    _filters_action = action

class catch_warnings:
    def __enter__(self):
        global _filters_action, _warnings_list
        self._old_action = _filters_action
        self._old_list = _warnings_list
        _warnings_list = []
        return self

    def __exit__(self, *args):
        global _filters_action, _warnings_list
        _filters_action = self._old_action
        _warnings_list = self._old_list
        return False
